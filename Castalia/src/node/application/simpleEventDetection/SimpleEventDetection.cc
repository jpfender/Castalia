#include "SimpleEventDetection.h"

Define_Module(SimpleEventDetection);

void SimpleEventDetection::startup()
{
	maxSampleInterval = ((double)par("maxSampleInterval")) / 1000.0;
	minSampleInterval = ((double)par("minSampleInterval")) / 1000.0;
	reportThreshold   = par("reportThreshold");
	bufferPeriod      = ((double)par("bufferPeriod")) / 1000.0;
	timestampEpsilon  = ((double)par("timestampEpsilon")) / 1000.0;
	filterOn 		  = par("filterOn");
	filterIdeal 	  = par("filterIdeal");
	removeOutliers    = par("removeOutliers");

	if (filterIdeal) {
		filterOn = true;
	}

	currSentSampleSN = 0;
	randomBackoffIntervalFraction = genk_dblrand(0);
	sentOnce = false;

	k = par("multiplicative_k");
	a = par("attenuation_exp_a");

	numSources = par("numSources");

	setTimer(REQUEST_SAMPLE, maxSampleInterval * randomBackoffIntervalFraction);

	//declareOutput("Packet filtering breakdown");

	declareOutput("True positive rate"); // (TP / P)
	declareOutput("False positive rate"); // (FP / N)

	declareOutput("ROC");
	declareOutput("Determinants");

	declareOutput("Sensitivity"); // (TP / (TP+FN))
	declareOutput("Specificity"); // (TN / (TN+FP))
	declareOutput("Precision"); // (TP / (TP + FP))
	declareOutput("Recall"); // (TP / (TP + FN))

	declareOutput("Accuracy"); // ((TP+TN) / (TP+TN+FP+FN))

	declareOutput("Base values");
	declareOutput("Final values");
}

void SimpleEventDetection::timerFiredCallback(int index)
{
	switch (index) {

		case REQUEST_SAMPLE:{
			requestSensorReading();
			setTimer(REQUEST_SAMPLE, maxSampleInterval);
			break;
		}

		case FLUSH_BUFFER:{
			while (buffer.size()) {

				SimpleEventDetectionDataPacket *refCandidate;
				vector<SimpleEventDetectionDataPacket*> candidates;

				refCandidate = buffer.back();
				simtime_t refCandidateTS = refCandidate->getExtraData().timestamp;
				candidates.push_back(refCandidate);
				buffer.pop_back();

				vector<SimpleEventDetectionDataPacket*>::iterator it;
				for (it = buffer.begin(); it != buffer.end(); ) {
					simtime_t currCandidateTS = (*it)->getExtraData().timestamp;
					if (refCandidateTS - timestampEpsilon <= currCandidateTS
							&& refCandidateTS + timestampEpsilon >= currCandidateTS) {
						candidates.push_back(*it);
						it = buffer.erase(it);
					} else {
						++it;
					}
				}

				filterAndSend(candidates);
			}

			isBuffering = false;
			break;
		}
	}
}

void SimpleEventDetection::fromNetworkLayer(ApplicationPacket
		* genericPacket, const char *source, double rssi, double lqi)
{
	int i;

	SimpleEventDetectionDataPacket *rcvPacket
		= check_and_cast<SimpleEventDetectionDataPacket*>(genericPacket);

	simpleEventReportData theData = rcvPacket->getExtraData();
	simtime_t nodeTS              = theData.timestamp;
	unsigned short nodeID         = theData.nodeID;

	if (isSink) {

		int i;

		simpleSourceInfo sources[numSources];
		for (i = 0; i < numSources; ++i) {
			sources[i] = rcvPacket->getSources(i);
		}

		double nodeX     = theData.locX;
		double nodeY     = theData.locY;

		packetsDeliveredToSink++;

		trace() << std::setprecision(2) << std::fixed;
		trace() << "[SINK] Got event report near (" << nodeX << "," << nodeY << ") @" << nodeTS;
		trace() << std::setprecision(2) << std::fixed;
		trace() << "[ORACLE] Actual event(s):";
		for (i = 0; i < numSources; ++i) {
			if (sources[i].ID != -1) {
				trace() << "\t#" << sources[i].ID << ": (" <<
					sources[i].x << "," << sources[i].y << ") @" <<
					sources[i].timestamp << " heard: " << sources[i].heard;

				if (sources[i].heard) {
					sinkEvents = addToEventInstanceVector(sinkEvents,
							sources[i].ID, sources[i].timestamp);
				}
			}
		}

	} else {

		if (filterOn) {

			if (!isBuffering) {
				setTimer(FLUSH_BUFFER, bufferPeriod);
				isBuffering = true;
			}

			vector<SimpleEventDetectionDataPacket*>::iterator it;
			for (it = buffer.begin(); it != buffer.end(); it++) {
				if ((*it)->getExtraData().nodeID == nodeID &&
						(*it)->getExtraData().timestamp == nodeTS) {
					return;
				}
			}
			buffer.push_back(rcvPacket->dup());

		} else {

			toNetworkLayer(rcvPacket->dup(), SINK_NETWORK_ADDRESS);
			collectOutput("Base values", "Packets sent");
		}
	}
}

void SimpleEventDetection::handleMessage(cMessage * msg)
{
	int msgKind = msg->getKind();

	if (disabled && msgKind != NODE_STARTUP)
	{
		delete msg;
		return;
	}

	if (msgKind == ORACLE) {
		SimpleEDPhysicalProcessMessage *oracleMsg
			= check_and_cast<SimpleEDPhysicalProcessMessage *>(msg);
		handleOracle(oracleMsg);

	} else {
		VirtualApplication::handleMessage(msg);
		return;
	}

	delete msg;
}

void SimpleEventDetection::handleOracle(SimpleEDPhysicalProcessMessage * oracleMsg)
{

	int i;

	currentOracle = new simpleSourceInfo[numSources];
	for (i = 0; i < numSources; ++i) {
		currentOracle[i].ID              = oracleMsg->getSources(i).ID;
		currentOracle[i].timestamp       = oracleMsg->getSources(i).timestamp;
		currentOracle[i].x               = oracleMsg->getSources(i).x;
		currentOracle[i].y               = oracleMsg->getSources(i).y;
		currentOracle[i].heard 			 = oracleMsg->getSources(i).heard;

		if (currentOracle[i].ID != -1 && currentOracle[i].heard) {
			groundTruthEvents
				= addToEventInstanceVector(groundTruthEvents,
						currentOracle[i].ID,
						currentOracle[i].timestamp);
		}
	}
	currSensingDistance = oracleMsg->getSensingDistance();
}

void SimpleEventDetection::handleSensorReading(SensorReadingMessage * rcvReading)
{
	int i;
	double sensValue = rcvReading->getSensedValue();

	if (sensValue < reportThreshold) {
		return;
	}

	simpleEventReportData tmpData;
	tmpData.nodeID    = (unsigned short)self;
	tmpData.timestamp = simTime();
	tmpData.locX      = mobilityModule->getLocation().x;
	tmpData.locY      = mobilityModule->getLocation().y;

	SimpleEventDetectionDataPacket *packet2Net = new
		SimpleEventDetectionDataPacket("Event reporting pck",
				APPLICATION_PACKET);
	packet2Net->setExtraData(tmpData);

	packet2Net->setSourcesArraySize(numSources);
	for (i = 0; i < numSources; ++i) {
		packet2Net->setSources(i, currentOracle[i]);
	}
	packet2Net->setSensingDistance(currSensingDistance);

	packet2Net->setData(sensValue);
	packet2Net->setSequenceNumber(currSentSampleSN);
	currSentSampleSN++;

	toNetworkLayer(packet2Net, SINK_NETWORK_ADDRESS);
	collectOutput("Base values", "Packets sent");
	packetsGenerated++;
	sentOnce = true;
}

/**
 * Check whether this event has been recorded before and, if not, add it
 * to the specified list of recorded events.
 */
vector<EventInstance> SimpleEventDetection::addToEventInstanceVector(vector<EventInstance>
		vec, int ID, simtime_t TS)
{
	vector<EventInstance>::iterator it;
	for (it = vec.begin(); it != vec.end(); it++) {
		if ((*it).ID == ID && (*it).TS - 2.5 <= TS && (*it).TS + 2.5 >= TS) {
			// We already have a recorded event with the same source ID
			// at roughly the same time; discard
			return vec;
		}
	}
	EventInstance event;
	event.ID = ID;
	event.TS = TS;
	vec.push_back(event);
	return vec;
}

/**
 * Perform a multilateration on all candidate measurements. If the
 * lateration succeeds (i.e. returns a valid result), treat all
 * candidate packets as duplicates and send only one of them. Otherwise,
 * remove the candidate furthest from the mean position, send it, and
 * try again with the rest until multilateration succeeds or there are
 * fewer than three candidates left (in which case send them out)
 */
void SimpleEventDetection::filterAndSend(vector<SimpleEventDetectionDataPacket*> candidates)
{
	vector<SimpleEventDetectionDataPacket*>::iterator it;
	simpleEventReportData theData;

	if (!removeOutliers) {
		if (!multilateration(candidates)) {
			for (it = candidates.begin(); it != candidates.end(); it++) {
				toNetworkLayer(*it, SINK_NETWORK_ADDRESS);
				collectOutput("Base values", "Packets sent");
			}
		} else {
			toNetworkLayer(candidates.back(), SINK_NETWORK_ADDRESS);
			collectOutput("Base values", "Packets sent");
		}
	} else {
		trace() << "[OUTLIER] Current candidates:";
		for (it = candidates.begin(); it != candidates.end(); it++) {
			theData = (*it)->getExtraData();
			trace() << "\t#" << theData.nodeID << " (" << theData.locX
				<< "," << theData.locY << ") @" << theData.timestamp;
		}
		while (candidates.size() >= 3) {
			if (!multilateration(candidates)) {
				trace() << "[OUTLIER] Multilateration failed";

				if (candidates.size() == 3) {
					// Send all candidates
					for (it = candidates.begin(); it != candidates.end(); it++) {
						toNetworkLayer(*it, SINK_NETWORK_ADDRESS);
						collectOutput("Base values", "Packets sent");
					}
					// Everything sent, nothing more to do
					return;
				}

				// Find mean position of all candidates
				float meanX = 0.0;
				float meanY = 0.0;
				for (it = candidates.begin(); it != candidates.end(); it++) {
					meanX += (*it)->getExtraData().locX;
					meanY += (*it)->getExtraData().locY;
				}
				meanX /= candidates.size();
				meanY /= candidates.size();
				trace() << "[OUTLIER] Mean candidate position: (" <<
					meanX << "," << meanY << ")";

				// Find point furthest from mean position
				SimpleEventDetectionDataPacket *curr_candidate = candidates.front();
				float curr_dist = 0.0;
				float max_dist = 0.0;
				float candX = 0.0;
				float candY = 0.0;
				int idx = 0;
				int cand_idx = 0;
				for (it = candidates.begin(); it != candidates.end(); it++) {
					candX = (*it)->getExtraData().locX;
					candY = (*it)->getExtraData().locY;
					curr_dist = sqrt((meanX - candX) * (meanX - candX) + (meanY
								- candY) * (meanY - candY));
					if (curr_dist > max_dist) {
						max_dist = curr_dist;
						curr_candidate = *it;
						cand_idx = idx;
					}
					idx++;
				}
				theData = curr_candidate->getExtraData();
				trace() << "[OUTLIER] Candidate identified for deletion:";
				trace() << "\t#" << theData.nodeID << " (" <<
					theData.locX << "," << theData.locY << ") @" <<
					theData.timestamp;

				// Send out outlier and delete it from candidates
				toNetworkLayer(curr_candidate, SINK_NETWORK_ADDRESS);
				collectOutput("Base values", "Packets sent");
				trace() << "[OUTLIER] Deletion index: " << cand_idx;
				candidates.erase(candidates.begin() + cand_idx);
				trace() << "[OUTLIER] Current candidates:";
				for (it = candidates.begin(); it != candidates.end(); it++) {
					theData = (*it)->getExtraData();
					trace() << "\t#" << theData.nodeID << " (" << theData.locX
						<< "," << theData.locY << ") @" << theData.timestamp;
				}
			} else {
				trace() << "[OUTLIER] Multilateration succeeded";
				// Multilateration succeeded; just send out one
				// representative and return
				toNetworkLayer(candidates.back(), SINK_NETWORK_ADDRESS);
				collectOutput("Base values", "Packets sent");
				return;
			}
		}
		trace() << "[OUTLIER] Fewer than three candidates";

		// candidates size can't be reduced; just send out everything
		for (it = candidates.begin(); it != candidates.end(); it++) {
			toNetworkLayer(*it, SINK_NETWORK_ADDRESS);
			collectOutput("Base values", "Packets sent");
		}
	}
}

bool SimpleEventDetection::multilateration(vector<SimpleEventDetectionDataPacket*> candidates)
{
	int i;
	bool isSameEvent;

	if (candidates.size() < 3) {
		return false;
	}

	eventsExamined++;
	collectOutput("Base values", "Events");

	vector<int> refSourceIDs;

	for (i = 0; i < numSources; i++) {
		simpleSourceInfo currSource = candidates.front()->getSources(i);
		if (currSource.ID != -1) {
			refSourceIDs.push_back(currSource.ID);
		}
	}

	PosAndDistanceVec beacons;
	Pos location;

	vector<SimpleEventDetectionDataPacket*>::iterator it;
	for (it = candidates.begin(); it != candidates.end(); it++) {

		double beaconX    = (*it)->getExtraData().locX;
		double beaconY    = (*it)->getExtraData().locY;
		double beaconDist = (*it)->getSensingDistance();

		simpleSourceInfo sources[numSources];

		isSameEvent = false;
		for (i = 0; i < numSources; ++i) {

			sources[i] = (*it)->getSources(i);

			vector<int>::iterator sit;
			for (sit = refSourceIDs.begin(); sit != refSourceIDs.end(); sit++) {
				if (sources[i].ID == (*sit)) {
					isSameEvent = true;
					break;
				}
			}
		}

		beacons.push_back(PosAndDistance(Pos(beaconX, beaconY), beaconDist));
	}

	if (filterIdeal) {
		if (isSameEvent) {
			positives++;
			collectOutput("Base values", "Positives");
			truePositives++;
			collectOutput("Base values", "True positives");
			return true;
		} else {
			negatives++;
			collectOutput("Base values", "Negatives");
			trueNegatives++;
			collectOutput("Base values", "True negatives");
			return false;
		}
	}

	size_t count = beacons.size();
	size_t rows = count * (count - 1) / 2;
	Eigen::MatrixXd m(rows, 2);
	Eigen::VectorXd b(rows);

	size_t row = 0;
	double x1, x2, y1, y2, r1, r2;
	PosAndDistance beacon1, beacon2;

	for (size_t i = 0; i < count; ++i) {
		beacon1 = beacons[i];
		for (size_t j = i+1; j < count; ++j) {
			beacon2 = beacons[j];

			x1 = beacon1.m_pos(0), y1 = beacon1.m_pos(1);
			x2 = beacon2.m_pos(0), y2 = beacon2.m_pos(1);
			r1 = beacon1.m_distance;
			r2 = beacon2.m_distance;
			m(row, 0) = x1 - x2;
			m(row, 1) = y1 - y2;
			b(row) = ((pow(x1, 2) - pow(x2, 2)) +
					(pow(y1, 2) - pow(y2, 2)) -
					(pow(r1, 2) - pow(r2, 2))) / 2;
			row++;
		}
	}

	location = m.jacobiSvd(Eigen::ComputeThinU|Eigen::ComputeThinV).solve(b);

	double eventX = location[0];
	double eventY = location[1];

	PosAndDistanceVec::iterator bit;
	for (bit = beacons.begin(); bit != beacons.end(); bit++) {

		double beaconX     = (*bit).m_pos(0);
		double beaconY     = (*bit).m_pos(1);
		double sensingDist = (*bit).m_distance;

		double distance = sqrt((beaconX - eventX) * (beaconX - eventX)
				+ (beaconY - eventY) * (beaconY - eventY));

		if (distance > sensingDist) {
			if (isSameEvent) {
				positives++;
				collectOutput("Base values", "Positives");
				falseNegatives++;
				collectOutput("Base values", "False negatives");
			} else {
				negatives++;
				collectOutput("Base values", "Negatives");
				trueNegatives++;
				collectOutput("Base values", "True negatives");
			}
			return false;
		}
	}

	if (isSameEvent) {
		positives++;
		collectOutput("Base values", "Positives");
		truePositives++;
		collectOutput("Base values", "True positives");
	} else {
		negatives++;
		collectOutput("Base values", "Negatives");
		falsePositives++;
		collectOutput("Base values", "False positives");
	}
	return true;
}

double SimpleEventDetection::norm(Pos p)
{
	return pow(pow(p[0], 2.0) + pow(p[1], 2.0), 0.5);
}

void SimpleEventDetection::finishSpecific()
{
	//if (packetsGenerated > 0) {
		//collectOutput("Packet filtering breakdown", "Packets generated", packetsGenerated);
		//collectOutput("Packet filtering breakdown", "Packets delivered to sink", packetsDeliveredToSink);
	//}

	if (isSink && groundTruthEvents.size() > 0) {
		float eventDeliveryRatio = (float) sinkEvents.size() / groundTruthEvents.size();
		trace() << "[OUTPUT] Event delivery ratio: " << eventDeliveryRatio;
		collectOutput("Determinants", "EDR", eventDeliveryRatio);
		sinkEvents.clear();
		groundTruthEvents.clear();
	}

	if (filterOn) {

		collectOutput("Final values", "Positives", positives);
		collectOutput("Final values", "Negatives", negatives);
		collectOutput("Final values", "True positives", truePositives);
		collectOutput("Final values", "True negatives", trueNegatives);
		collectOutput("Final values", "False positives", falsePositives);
		collectOutput("Final values", "False negatives", falseNegatives);

		if (positives + negatives > 37) { //minimum sample size for 95% confidence and 0.1 MOE

			if (positives > 0) {
				truePositiveRate = (float) truePositives / positives;
				sensitivity = recall = truePositiveRate;
				collectOutput("Determinants", "True positive rate", truePositiveRate);
			}

			if (negatives > 0) {
				falsePositiveRate = (float) falsePositives / negatives;
				collectOutput("Determinants", "False positive rate", falsePositiveRate);
			}

			if (trueNegatives + falsePositives > 0) {
				specificity = (float) trueNegatives / (trueNegatives
						+ falsePositives);
				collectOutput("Determinants", "Specificity", specificity);
			}

			if (truePositives + falsePositives > 0) {
				precision = (float) truePositives / (truePositives + falsePositives);
				collectOutput("Determinants", "Precision", precision);
			}

			//collectOutput("Determinants", "Youden index", sensitivity + specificity - 1);

			//if (precision + recall > 0) {
				//float f1  = 2 	 * ((precision * recall) / (        precision  + recall));
				//float f2  = 5    * ((precision * recall) / ((4    * precision) + recall));
				//float f05 = 1.25 * ((precision * recall) / ((1.25 * precision) + recall));
				//collectOutput("Determinants", "F1 score", f1);
				//collectOutput("Determinants", "F2 score", f2);
				//collectOutput("Determinants", "F0.5 score", f05);
			//}

			if (truePositives + trueNegatives + falsePositives + falseNegatives > 0) {
				collectOutput("Determinants", "Accuracy", (float)
						(truePositives + trueNegatives) / (truePositives
							+ trueNegatives + falsePositives
							+ falseNegatives));
			}
		}
	}
}
