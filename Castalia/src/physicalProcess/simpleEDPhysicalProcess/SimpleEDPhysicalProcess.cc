#include "SimpleEDPhysicalProcess.h"

Define_Module(SimpleEDPhysicalProcess);

void SimpleEDPhysicalProcess::initialize()
{
    sensingDistance = par("sensingDistance");
    CustomizablePhysicalProcess::initialize();
}

void SimpleEDPhysicalProcess::handleMessage(cMessage *msg)
{

    if (msg->getKind() != PHYSICAL_PROCESS_SAMPLING)
	opp_error("Physical Process received message other than PHYSICAL_PROCESS_SAMPLING");

    PhysicalProcessMessage *receivedMsg = check_and_cast<PhysicalProcessMessage *>(msg);

    SimpleEDPhysicalProcessMessage *oracleMsg = new
	SimpleEDPhysicalProcessMessage("oracle", ORACLE);

    oracleMsg->setSrcID(receivedMsg->getSrcID());
    oracleMsg->setSensorIndex(receivedMsg->getSensorIndex());
    oracleMsg->setXCoor(receivedMsg->getXCoor());
    oracleMsg->setYCoor(receivedMsg->getYCoor());
    oracleMsg->setSensingDistance(sensingDistance);

    int i;
    int nodeIndex = receivedMsg->getSrcID();
    double returnValue;

    returnValue = CustomizablePhysicalProcess::calculateScenarioReturnValue(receivedMsg->getXCoor(),
            receivedMsg->getYCoor(), receivedMsg->getSendingTime());

    // Record sensor value
    receivedMsg->setValue(returnValue);

    sources = oracle(receivedMsg->getXCoor(), receivedMsg->getYCoor(),
	    receivedMsg->getSendingTime());

    if (distanceCutoff(receivedMsg->getXCoor(), receivedMsg->getYCoor())) {

	// Record info about sources in range
	oracleMsg->setSourcesArraySize(numSources);
	for (i = 0; i < numSources; ++i) {
	    oracleMsg->setSources(i, sources[i]);
	}
	oracleMsg->setValue(returnValue);

	// Send oracle information directly back to application
	send(oracleMsg, "groundTruthToNode", nodeIndex);

	// Send reply back to the node that made the request
	send(receivedMsg, "toNode", nodeIndex);

    }
}

/**
 * Returns true if at least one active source is within the sensing
 * distance; false otherwise.
 */
bool SimpleEDPhysicalProcess::distanceCutoff(const double &x_coo, const
	double &y_coo)
{
    bool retVal = false;
    int i = 0;
    double distance, sourceX, sourceY;
    for (i = 0; i < numSources; ++i) {
        if (source_index[i] >= 0 && curr_source_state[i].value == 30) {

	    sourceX = curr_source_state[i].x;
	    sourceY = curr_source_state[i].y;

	    distance = sqrt((x_coo - sourceX) * (x_coo - sourceX)
		    + (y_coo - sourceY) * (y_coo - sourceY));

	    if (distance <= sensingDistance) {
		sources[i].heard = true;
		retVal = true;
	    }

        }
    }

    return retVal;
}

sourceOracle *SimpleEDPhysicalProcess::oracle(const double &x_coo, const
	double &y_coo, const simtime_t & stime)
{
    int i;
    double currVal, distance, sourceX, sourceY;
    sources = new sourceOracle[numSources];

    for (i = 0; i < numSources; i++) {
	if (source_index[i] >= 0 && curr_source_state[i].value == 30) {

	    sourceX = curr_source_state[i].x;
	    sourceY = curr_source_state[i].y;

	    distance = sqrt((x_coo - sourceX) * (x_coo - sourceX)
		+ (y_coo - sourceY) * (y_coo - sourceY));

	    if (distance <= sensingDistance && !(stime == 0 && sourceX == 0 && sourceY == 0)) {

		sources[i].ID        = i;
		sources[i].timestamp = stime;
		sources[i].x         = sourceX;
		sources[i].y         = sourceY;

	    } else {

		// Make entry invalid
	        sources[i].ID = -1;
		sources[i].timestamp = -1;
		sources[i].x = -1;
		sources[i].y = -1;
	    }
	} else {
	    // Make entry invalid
	    sources[i].ID = -1;
	    sources[i].timestamp = -1;
	    sources[i].x = -1;
	    sources[i].y = -1;
	}
	sources[i].heard = false;
    }

    return sources;
}
