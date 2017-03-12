#include "LocalizationPhysicalProcess.h"

Define_Module(LocalizationPhysicalProcess);

void LocalizationPhysicalProcess::handleMessage(cMessage *msg)
{
    if (msg->getKind() != PHYSICAL_PROCESS_SAMPLING)
	opp_error("Physical Process received message other than PHYSICAL_PROCESS_SAMPLING");

    PhysicalProcessMessage *receivedMsg = check_and_cast<PhysicalProcessMessage *>(msg);

    LocalizationPhysicalProcessMessage *groundTruthMsg = new
	LocalizationPhysicalProcessMessage("ground truth",
		GROUND_TRUTH);

    groundTruthMsg->setSrcID(receivedMsg->getSrcID());
    groundTruthMsg->setSensorIndex(receivedMsg->getSensorIndex());
    groundTruthMsg->setXCoor(receivedMsg->getXCoor());
    groundTruthMsg->setYCoor(receivedMsg->getYCoor());

    int nodeIndex = receivedMsg->getSrcID();
    double returnValue;
    sourceInfo si;

    returnValue = CustomizablePhysicalProcess::calculateScenarioReturnValue(receivedMsg->getXCoor(),
            receivedMsg->getYCoor(), receivedMsg->getSendingTime());

    // Record sensor value
    receivedMsg->setValue(returnValue);

    si = getStrongestSource(receivedMsg->getXCoor(),
            receivedMsg->getYCoor());

    // Record info about strongest source
    groundTruthMsg->setInfo(si);
    groundTruthMsg->setValue(returnValue);

    // Send ground truth information back to application
    send(groundTruthMsg, "groundTruthToNode", nodeIndex);

    // Send reply back to the node who made the request
    send(receivedMsg, "toNode", nodeIndex);
}

sourceInfo LocalizationPhysicalProcess::getStrongestSource(const double
	&x_coo, const double &y_coo)
{
    int i;
    double highestVal, currVal, retVal, linear_coeff, distance, sourceX, sourceY;
    sourceInfo si;

    highestVal = 0.0f;
    for (i = 0; i < numSources; i++) {
	if (source_index[i] >= 0 && curr_source_state[i].value == 30) {

	    sourceX = curr_source_state[i].x;
	    sourceY = curr_source_state[i].y;

	    distance = sqrt((x_coo - sourceX) * (x_coo - sourceX)
		+ (y_coo - sourceY) * (y_coo - sourceY));
	    distance = roundf(distance * 10000) / 10000;

	    currVal = pow(k * distance + 1, -a) * curr_source_state[i].value;

	    if (currVal >= highestVal) {

		si.ID = i;
		si.x = sourceX;
		si.y = sourceY;
		si.sensedValue = currVal;

		highestVal = currVal;
	    }
	}
    }

    return si;
}
