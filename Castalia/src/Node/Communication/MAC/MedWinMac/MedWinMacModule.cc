/********************************************************************************
 *  Copyright: National ICT Australia,  2010                                    *
 *  Developed at the ATP lab, Networked Systems theme                           *
 *  Author(s): Athanassios Boulis, Yuri Tselishchev                             *
 *  This file is distributed under the terms in the attached LICENSE file.      *
 *  If you do not find this file, copies can be found by writing to:            *
 *                                                                              *
 *      NICTA, Locked Bag 9013, Alexandria, NSW 1435, Australia                 *
 *      Attention:  License Inquiry.                                            *
 ********************************************************************************/

#include "MedWinMacModule.h"

Define_Module(MedWinMacModule);

void MedWinMacModule::startup() {
	isHub = par("isHub");
	if (isHub) {
		connectedHID = SELF_MAC_ADDRESS % 2<<16;
		connectedNID = BROADCAST_NID; // default value, usually overwritten
		currentFreeConnectedNID = 16; // start assigning connected NID from ID 16
		allocationSlotLength = (double) par("allocationSlotLength")/1000.0; // convert msec to sec
		beaconPeriodLength = par("beaconPeriodLength");
		RAP1Length = currentScheduleAssignmentStart = par("RAP1Length");
		setTimer(SEND_BEACON,0);
	} else {
		connectedHID = UNCONNECTED;
 		connectedNID = UNCONNECTED;
		unconnectedNID = 1 + genk_intrand(0,14);    //we select random unconnected NID
		scheduledAccessLength = par("scheduledAccessLength");
		scheduledAccessPeriod = par("scheduledAccessPeriod");
		pastSyncIntervalNominal = false;
		macState = MAC_SETUP;
		SInominal = -1;
	}

	pTIFS = (double) par("pTIFS")/1000.0;
	phyLayerOverhead = par("phyLayerOverhead");
	phyDataRate = par("phyDataRate");
	priority = getParentModule()->getParentModule()->getSubmodule("nodeApplication")->par("priority");
	mClockAccuracy = par("mClockAccuracy");

	contentionSlotLength = (double) par("contentionSlotLength")/1000.0; // convert msec to sec;
	maxPacketRetries = par("maxPacketRetries");
	CW = CWmin[priority];
	CWdouble = false;
}

void MedWinMacModule::timerFiredCallback(int index) {
	switch (index) {

		case CARRIER_SENSING: {
			if (!canFitTx()) break;

			CCA_result CCAcode = radioModule->isChannelClear();
			if (CCAcode == CLEAR) {
				backoffCounter--;
				if (backoffCounter > 0) setTimer(CARRIER_SENSING, contentionSlotLength);
				else sendPacket();
			} else {
				/* spec states that we wait until the channel is not busy
				 * we cannot simply do that, we have to have periodic checks
				 * we arbitrarily choose 10*contention slot = 3.6msec
				 */
				setTimer(CARRIER_SENSING, contentionSlotLength*10.0);
			}
			break;
		}

		case SEND_PACKET: {
			if (needToTx() && canFitTx()) sendPacket();
			break;
		}

		case ACK_TIMEOUT: {

			// double the Contention Window, after every second fail.
			CWdouble ? CWdouble=false : CWdouble=true;
			if ((CWdouble) && (CW < CWmax[priority])) CW *=2;

			if (!needToTx()) break;
			if (macState == MAC_RAP) attemptTxInRAP();
			if ((macState == MAC_SCHEDULED_TX_ACCESS) && (canFitTx())) sendPacket();
			break;
		}

		case START_SLEEPING: {
			macState = MAC_SLEEP;
			toRadioLayer(createRadioCommand(SET_STATE,SLEEP));
			break;
		}

		case START_SCHEDULED_TX_ACCESS: {
			macState = MAC_SCHEDULED_TX_ACCESS;
			endTime = getClock() + (scheduledAccessEnd - scheduledAccessStart) * allocationSlotLength;
			if (beaconPeriodLength > scheduledAccessEnd)
				setTimer(START_SLEEPING, (scheduledAccessEnd - scheduledAccessStart) * allocationSlotLength);
			if (needToTx() && canFitTx()) sendPacket();
			break;
		}

		case WAKEUP_FOR_BEACON: {
			macState = MAC_BEACON_WAIT;
			toRadioLayer(createRadioCommand(SET_STATE,RX));
			break;
		}

		case SYNC_INTERVAL_TIMEOUT: {
			pastSyncIntervalNominal = true;
			syncIntervalAdditionalStart = getClock();
			break;
		}

		// These timers are specific to a Hub
		case SEND_BEACON: {
			macState = MAC_RAP;
			setTimer(SEND_BEACON,beaconPeriodLength * allocationSlotLength);
			setTimer(HUB_SCHEDULED_ACCESS, RAP1Length * allocationSlotLength);

			MedWinBeaconPacket * beaconPkt = new MedWinBeaconPacket("MedWin beacon",MAC_LAYER_PACKET);
			setHeaderFields(beaconPkt,N_ACK_POLICY,MANAGEMENT,BEACON);
			beaconPkt->setNID(BROADCAST_NID);

			beaconPkt->setAllocationSlotLength(allocationSlotLength);
			beaconPkt->setBeaconPeriodLength(beaconPeriodLength);
			beaconPkt->setRAP1Length(RAP1Length);

			toRadioLayer(beaconPkt);
			toRadioLayer(createRadioCommand(SET_STATE,TX));

			setTimer(HUB_ATTEMPT_TX_IN_RAP, pTIFS + TX_TIME(beaconPkt->getByteLength()));
			break;
		}

		case HUB_ATTEMPT_TX_IN_RAP: {
			macState = MAC_RAP;
			if (needToTx()) attemptTxInRAP();
			break;
		}

		case HUB_SCHEDULED_ACCESS: {
			macState = MAC_SCHEDULED_RX_ACCESS;
			// we should look at the schedule and setup timers to get in and out
			// of MAC_SCHEDULED_RX_ACCESS MAC_SCHEDULED_TX_ACCESS and finally MAC_SLEEP
			break;
		}
	}
}

void MedWinMacModule::fromNetworkLayer(cPacket *pkt, int dst) {
	MedWinMacPacket *medWinDataPkt = new MedWinMacPacket("MedWin data packet",MAC_LAYER_PACKET);
	encapsulatePacket(medWinDataPkt,pkt);
	if (bufferPacket(medWinDataPkt)) {
		if (packetToBeSent == NULL && needToTx()) {
			if (macState == MAC_RAP) attemptTxInRAP();
			if ((macState == MAC_SCHEDULED_TX_ACCESS) && (canFitTx())) sendPacket();
		}
	} else {
		trace() << "WARNING MedWin MAC buffer overflow";
		collectOutput("Buffer overflow");
	}
}

void MedWinMacModule::fromRadioLayer(cPacket *pkt, double rssi, double lqi) {
	// if the incoming packet is not MedWin, return (VirtualMAC will delete it)
	MedWinMacPacket * medWinPkt = dynamic_cast<MedWinMacPacket*>(pkt);
	if (medWinPkt == NULL) return;

	// filter the incoming MedWin packet
    if (!isPacketForMe(medWinPkt)) return;

	// if the packet received requires an ACK, we should send it now
	if (medWinPkt->getAckPolicy() == I_ACK_POLICY) {
		MedWinMacPacket * ackPacket = new MedWinMacPacket("ACK packet",MAC_LAYER_PACKET);
		setHeaderFields(ackPacket,N_ACK_POLICY,CONTROL,I_ACK);
		ackPacket->setNID(medWinPkt->getNID());
		toRadioLayer(ackPacket);
		toRadioLayer(createRadioCommand(SET_STATE,TX));
	}

	if (medWinPkt->getFrameType() == DATA) {
		toNetworkLayer(decapsulatePacket(medWinPkt));
		// if this pkt requires a block ACK we should send it,
		// by looking what packet we have received
		// NOT IMPLEMENTED
		if (medWinPkt->getAckPolicy() == B_ACK_POLICY){
		}
		return;
	}

	switch(medWinPkt->getFrameSubtype()) {
		case BEACON: {
			MedWinBeaconPacket * medWinBeacon = check_and_cast<MedWinBeaconPacket*>(medWinPkt);
			simtime_t beaconTxTime = TX_TIME(medWinBeacon->getByteLength());

			// get the allocation slot length, which is used in many calculations
			allocationSlotLength = medWinBeacon->getAllocationSlotLength() / 1000.0;
			SInominal = (allocationSlotLength/10.0 - 2*pTIFS) / 2*mClockAccuracy;

			// a beacon is our synchronization event. Update relevant timer
			setTimer(SYNC_INTERVAL_TIMEOUT, SInominal);

			beaconPeriodLength = medWinBeacon->getBeaconPeriodLength();
			RAP1Length = medWinBeacon->getRAP1Length();
			if (RAP1Length > 0) {
				macState = MAC_RAP;
				endTime = getClock() + RAP1Length * allocationSlotLength - beaconTxTime;
			}

			if (connectedHID == UNCONNECTED) {
				/* We will try to connect to this BAN  if our scheduled access length
				 * is NOT set to unconnected (-1). If it is set to 0, it means we are
				 * establishing a sleeping pattern and waking up only to hear beacons
				 * and are only able to transmit in RAP periods.
				 */
				if (scheduledAccessLength >= 0) {
					// we are unconnected, and we need to connect to obtain scheduled access
					// we will create and send a connection request
					MedWinConnectionRequestPacket *connectionRequest = new MedWinConnectionRequestPacket("MedWin connection request packet",MAC_LAYER_PACKET);

					// This block takes care of general header fields
					setHeaderFields(connectionRequest,I_ACK_POLICY,MANAGEMENT,CONNECTION_REQUEST);
					// while setHeaderFields should take care of the HID field, we are currently unconnected.
					// We want to keep this state, yet send the request to the right hub.
					connectionRequest->setHID(medWinBeacon->getHID());

					// This block takes care of connection request specific fields
					connectionRequest->setRecipientAddress(medWinBeacon->getSenderAddress());
					connectionRequest->setSenderAddress(SELF_MAC_ADDRESS);
					// in this implementation our schedule always starts from the next beacon
					connectionRequest->setNextWakeup(medWinBeacon->getSequenceNumber() + 1);
					connectionRequest->setWakeupInterval(scheduledAccessPeriod);
					//uplink request is simplified in this implementation to only ask for a number of slots needed
					connectionRequest->setUplinkRequest(scheduledAccessLength);

					if (packetToBeSent) cancelAndDelete(packetToBeSent);
					packetToBeSent = connectionRequest;
					currentPacketRetries = 0;
					attemptTxInRAP();
				} else if (needToTx()) {
					attemptTxInRAP();
				}

			// else we are connected already and previous filtering made sure
 			// that this beacon belongs to our BAN
			} else  {

				// schedule a timer to wake up for the next beacon (it might be m periods away
				setTimer(WAKEUP_FOR_BEACON, beaconPeriodLength * scheduledAccessPeriod * allocationSlotLength - beaconTxTime - GUARD_TIME );

				// if we have a schedule that does not start after RAP, or our schedule
				// is not assigned yet, then go to sleep after RAP.  RAP === period length???
				if ((scheduledAccessStart == UNCONNECTED && RAP1Length < beaconPeriodLength)
								|| (scheduledAccessStart!= RAP1Length +1))
					setTimer(START_SLEEPING, RAP1Length * allocationSlotLength - beaconTxTime);

				// schedule the timer to go in scheduled access
				if (scheduledAccessStart != UNCONNECTED) {
					setTimer(START_SCHEDULED_TX_ACCESS, scheduledAccessStart * allocationSlotLength - beaconTxTime - GUARD_TIME);
				}

				// We are in Tx if you can
				if (needToTx()) attemptTxInRAP();
			}

			break;
		}

		case I_ACK_POLL: {
			// handle the polling part and not use 'break' to let it roll over to the ACK part
		}

		case I_ACK: {
			cancelTimer(ACK_TIMEOUT);
			cancelAndDelete(packetToBeSent);
			packetToBeSent = NULL;
			CW = CWmin[priority];
			if (!needToTx()) break;
			if (macState == MAC_RAP) attemptTxInRAP();
			if ((macState == MAC_SCHEDULED_TX_ACCESS) && (canFitTx())) sendPacket();
			break;
		}

		case B_ACK_POLL: {
			// handle the polling part and not use 'break' to let it roll over to the ACK part
		}

		case B_ACK: {
			cancelTimer(ACK_TIMEOUT);
			cancelAndDelete(packetToBeSent);
			packetToBeSent = NULL;
			CW = CWmin[priority];

			// we need to analyze the bitmap and see if some of the LACK packets need to be retxed
			if (!needToTx()) break;
			if (macState == MAC_RAP) attemptTxInRAP();
			if ((macState == MAC_SCHEDULED_TX_ACCESS) && (canFitTx())) sendPacket();
			break;
		}

		case CONNECTION_ASSIGNMENT: {
			MedWinConnectionAssignmentPacket *connAssignment = check_and_cast<MedWinConnectionAssignmentPacket*>(medWinPkt);
			if (connAssignment->getRecipientAddress() != MAC_SELF_ADDRESS) {
				if (unconnectedNID == connAssignment->getNID()) unconnectedNID = 1 + genk_intrand(0,14);
				break;
			}
			if (connAssignment->getStatusCode() == ACCEPTED || connAssignment->getStatusCode() == MODIFIED) {
				connectedHID = connAssignment->getHID();
				connectedNID = connAssignment->getNID();
				scheduledAccessStart = connAssignment->getUplinkRequestStart();
				scheduledAccessEnd = connAssignment->getUplinkRequestEnd();
			} // else we dont need to do anything - request is rejected

			break;
		}

		case DISCONNECTION: {
			connectedHID = UNCONNECTED;
			connectedNID = UNCONNECTED;
			break;
		}

		case CONNECTION_REQUEST: {
			MedWinConnectionRequestPacket *connRequest = check_and_cast<MedWinConnectionRequestPacket*>(medWinPkt);

			/* We acked the packet. Now it is a design decision when to send
			 * connection assignment. Here we send it immediately.
			 */
			MedWinConnectionAssignmentPacket *connAssignment = new MedWinConnectionAssignmentPacket("MedWin connection assignment",MAC_LAYER_PACKET);
			setHeaderFields(connAssignment,I_ACK_POLICY,MANAGEMENT,CONNECTION_ASSIGNMENT);
			connAssignment->setNID(currentFreeConnectedNID);

			connAssignment->setRecipientAddress(connRequest->getSenderAddress());
			if (connRequest->getUplinkRequest() > beaconPeriodLength - currentScheduleAssignmentStart) {
				connAssignment->setStatusCode(REJ_NO_RESOURCES);
				// can not accomodate request
			} else if (currentFreeConnectedNID > 239) {
				connAssignment->setStatusCode(REJ_NO_NID);
			} else {
				connAssignment->setStatusCode(ACCEPTED);
				currentFreeConnectedNID++;
				connAssignment->setUplinkRequestStart(currentScheduleAssignmentStart);
				connAssignment->setUplinkRequestEnd(currentScheduleAssignmentStart + connRequest->getUplinkRequest());
				currentScheduleAssignmentStart += connRequest->getUplinkRequest() + 1;
			}

			toRadioLayer(connAssignment);

			// the MEDWIN_HEADER_SIZE below is the size of an ACK packet
			setTimer(ACK_TIMEOUT, 2*TX_TIME(MEDWIN_HEADER_SIZE) + 3*pTIFS + TX_TIME(connAssignment->getByteLength()));
		}

		case POLL:
		case T_POLL:

		case ASSOCIATION:
		case DISASSOCIATION:
		case PTK:
		case GTK: {
			trace() << "WARNING: unimplemented packet subtype in [" << medWinPkt->getName() << "]";
			break;
		}
	}
}

bool MedWinMacModule::isPacketForMe(MedWinMacPacket *pkt) {
	int pktHID = pkt->getHID();
	int pktNID = pkt->getNID();

	if (connectedHID == pktHID) {
		if (isHub) return true;
		if ((connectedNID == pktNID) || (pktNID == BROADCAST_NID)) return true;
		if (connectedNID != UNCONNECTED) return false;
		if ((unconnectedNID == pktNID) || (pktNID == UNCONNECTED_BROADCAST_NID)) return true;
	} else if (connectedHID == UNCONNECTED) return true;

	// for all other cases return false
	return false;
}

simtime_t MedWinMacModule::guardTime() {
	return (simtime_t) allocationSlotLength / 10.0 + (getClock() - syncIntervalAdditionalStart) * mClockAccuracy;
}

void MedWinMacModule::setHeaderFields(MedWinMacPacket * pkt, AcknowledgementPolicy_type ackPolicy, Frame_type frameType, Frame_subtype frameSubtype) {
	pkt->setHID(connectedHID);
	if (connectedNID != UNCONNECTED)
		pkt->setNID(connectedNID);
	else
		pkt->setNID(unconnectedNID);

	pkt->setAckPolicy(ackPolicy);
	pkt->setFrameType(frameType);
	pkt->setFrameSubtype(frameSubtype);
}

void MedWinMacModule::attemptTxInRAP() {
	if (backoffCounter == 0) {
		backoffCounter = 1 + genk_intrand(0,CW);
	}
	setTimer(CARRIER_SENSING,0);
}

// This function will examine the current packet ...
//
bool MedWinMacModule::needToTx() {
	if (packetToBeSent) {
		if (currentPacketRetries < maxPacketRetries) return true;
		cancelAndDelete(packetToBeSent);
		packetToBeSent = NULL;
	}

	if (TXBuffer.size() == 0) return false;
	if (macState != MAC_RAP && macState != MAC_SCHEDULED_TX_ACCESS) return false;

	packetToBeSent = (MedWinMacPacket*)TXBuffer.front();   TXBuffer.pop();
	setHeaderFields(packetToBeSent, I_ACK_POLICY, DATA, RESERVED);
	currentPacketRetries = 0;
	return true;
}

bool MedWinMacModule::canFitTx() {
	if ( endTime - getClock() - GUARD_TIME - TX_TIME(packetToBeSent->getByteLength()) > 0) return true;
	return false;
}

void MedWinMacModule::sendPacket() {
	toRadioLayer(packetToBeSent->dup());
	toRadioLayer(createRadioCommand(SET_STATE,TX));
	currentPacketRetries++;

	if (packetToBeSent->getAckPolicy() == I_ACK || packetToBeSent->getAckPolicy() == B_ACK) {
		// need to wait for ack
		setTimer(ACK_TIMEOUT, TX_TIME(packetToBeSent->getByteLength()) + 2*pTIFS + TX_TIME(MEDWIN_HEADER_SIZE));
	} else {
		// no need to wait
		setTimer(SEND_PACKET, TX_TIME(packetToBeSent->getByteLength()));
	}
}

simtime_t MedWinMacModule::timeToNextBeacon(simtime_t interval, int index, int phase) {
	return interval;
}