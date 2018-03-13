#include "AnimalPhysicalProcess.h"

Define_Module(AnimalPhysicalProcess);

void AnimalPhysicalProcess::initialize()
{
    sensingDistance = par("sensingDistance");
    field_x = par("field_x");
    field_y = par("field_y");
    k = par("multiplicative_k");
    a = par("attenuation_exp_a");
    numAnimals = par("numAnimals");
    if (numAnimals > 5)
	    opp_error("Physical Process parameter \"numAnimals\" has been initialized with invalid value \"%d\"",
		    numAnimals);

    int i, j;
    animals = new animal[numAnimals];
    trace() << "[ANIMALS] Initialising animals...";
    for (i = 0; i < numAnimals; i++) {
	animals[i].x            = (int) (genk_dblrand(0) * 1000) % field_x;
	animals[i].y            = (int) (genk_dblrand(0) * 1000) % field_y;
	animals[i].value        = 30;
	animals[i].state        = GRAZING;
	animals[i].goal[0]      = (int) (genk_dblrand(0) * 1000) % field_x;
	animals[i].goal[1]      = (int) (genk_dblrand(0) * 1000) % field_y;
	animals[i].when_thirsty = (simtime_t) ((int) (genk_dblrand(0) * 1000) % 600);

	trace() << "[ANIMALS] Animal " << i << ": pos (" << animals[i].x
	    << "," << animals[i].y << ") goal (" << animals[i].goal[0]
	    << "," << animals[i].goal[1] << ") will become thirsty at: "
	    << animals[i].when_thirsty;
    }

    watering_hole_x = (int) (genk_dblrand(0) * 1000) % field_x;
    watering_hole_y = (int) (genk_dblrand(0) * 1000) % field_y;

    trace() << "[ANIMALS] Watering hole at (" << watering_hole_x << "," << watering_hole_y << ")";

    scheduleAt(1, new cMessage("Animal update", TIMER_SERVICE));
}

void AnimalPhysicalProcess::handleMessage(cMessage *msg)
{

    switch (msg->getKind()) {
	case PHYSICAL_PROCESS_SAMPLING: {
		 
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

	    returnValue = AnimalPhysicalProcess::calculateScenarioReturnValue(receivedMsg->getXCoor(),
		    receivedMsg->getYCoor(), receivedMsg->getSendingTime());

	    // Record sensor value
	    receivedMsg->setValue(returnValue);

	    sources = oracle(receivedMsg->getXCoor(), receivedMsg->getYCoor(),
		    receivedMsg->getSendingTime());

	    if (distanceCutoff(receivedMsg->getXCoor(), receivedMsg->getYCoor())) {

		// Record info about sources in range
		oracleMsg->setSourcesArraySize(numAnimals);
		for (i = 0; i < numAnimals; ++i) {
		    oracleMsg->setSources(i, sources[i]);
		}
		oracleMsg->setValue(returnValue);

		// Send oracle information directly back to application
		send(oracleMsg, "groundTruthToNode", nodeIndex);

		// Send reply back to the node that made the request
		send(receivedMsg, "toNode", nodeIndex);

	    }

	    return;
	}
	
	case TIMER_SERVICE: {

	    int i;
	    for (i = 0; i < numAnimals; i++) {
		trace() << "[ANIMALS] Updating animal " << i << " state";
		updateAnimalState(&animals[i]);
	    }

	    scheduleAt(simTime() + 1, msg);

	    return;
	}

	default: {
	    opp_error(":\n Physical Process received message other than PHYSICAL_PROCESS_SAMPLING");
	}

    }

}

double AnimalPhysicalProcess::calculateScenarioReturnValue(const double
	&x_coo, const double &y_coo, const simtime_t & stime)
{
    int i;
    double retVal, linear_coeff, distance;

    // XXX state calculation was here -- we don't need it, right?

    /* Now that we know the current state of your process calculate its effect on all the nodes */
    // add all active sources
    retVal = 0.0f;
    for (i = 0; i < numAnimals; i++) {
	distance = sqrt((x_coo - animals[i].x) * (x_coo - animals[i].x)
		+ (y_coo - animals[i].y) * (y_coo - animals[i].y));
	retVal += pow(k * distance + 1, -a) * animals[i].value;
    }

    return retVal;
}

/**
 * Returns true if at least one active animal is within the sensing
 * distance; false otherwise.
 */
bool AnimalPhysicalProcess::distanceCutoff(const double &x_coo, const
	double &y_coo)
{
    bool retVal = false;
    int i = 0;
    double distance, animalX, animalY;
    for (i = 0; i < numAnimals; ++i) {

	animalX = animals[i].x;
	animalY = animals[i].y;

	distance = sqrt((x_coo - animalX) * (x_coo - animalX) + (y_coo
		    - animalY) * (y_coo - animalY));

	if (distance <= sensingDistance) {
	    animals[i].heard = true;
	    retVal = true;
	}

    }

    return retVal;
}

sourceOracle *AnimalPhysicalProcess::oracle(const double &x_coo, const
	double &y_coo, const simtime_t & stime)
{
    int i;
    double currVal, distance, sourceX, sourceY;
    sources = new sourceOracle[numAnimals];

    for (i = 0; i < numAnimals; i++) {

	sourceX = animals[i].x;
	sourceY = animals[i].y;

	distance = sqrt((x_coo - sourceX) * (x_coo - sourceX)
	    + (y_coo - sourceY) * (y_coo - sourceY));

	if (distance <= sensingDistance && !(stime == 0 && sourceX == 0 && sourceY == 0)) {

	    sources[i].ID        = i;
	    sources[i].timestamp = stime;
	    sources[i].x         = sourceX;
	    sources[i].y         = sourceY;
	    sources[i].heard 	 = true;

	} else {

	    // Make entry invalid
	    sources[i].ID = -1;
	    sources[i].timestamp = -1;
	    sources[i].x = -1;
	    sources[i].y = -1;
	    sources[i].heard = false;
	}
	//sources[i].heard = false;
    }

    return sources;
}

double AnimalPhysicalProcess::distToGoal(animal *a)
{
    return sqrt((a->goal[0] - a->x) * (a->goal[0] - a->x) + (a->goal[1]
		- a->y) * (a->goal[1] - a->y));
}

void AnimalPhysicalProcess::updateAnimalState(animal *a)
{
    double transition = genk_dblrand(0);
    double cur_speed;

    if (a->when_thirsty == simTime()) {

	a->state   = THIRSTY;
	a->goal[0] = watering_hole_x;
	a->goal[1] = watering_hole_y;

	trace() << "[ANIMAL] Became thirsty!";
    }

    switch (a->state) {
	case GRAZING:
	    cur_speed = GRAZING_SPEED;
	    break;
	case WALKING:
	case THIRSTY:
	    cur_speed = WALKING_SPEED;
	    break;
	case RUNNING:
	    cur_speed = RUNNING_SPEED;
	    break;
	default:
	    cur_speed = GRAZING_SPEED;
    }

    trace() << "[ANIMALS] Current goal: (" << a->goal[0] << "," << a->goal[1] << ")";

    while (distToGoal(a) < cur_speed) {

	a->goal[0] = (int) (genk_dblrand(0) * 1000) % field_x;
	a->goal[1] = (int) (genk_dblrand(0) * 1000) % field_y;

	if (a->state == THIRSTY) {
	    a->state = GRAZING;
	    trace() << "[ANIMAL] No longer thirsty!";
	}

	trace() << "[ANIMALS] Goal changed to: (" << a->goal[0] << "," << a->goal[1] << ")";
    }

    trace() << "[ANIMALS] Current position: (" << a->x << "," << a->y << ")";

    if (a->x < a->goal[0]) {
	a->x += cur_speed;
    } else if (a->x > a->goal[0]) {
	a->x -= cur_speed;
    }

    if (a->y < a->goal[1]) {
	a->y += cur_speed;
    } else if (a->y > a->goal[1]) {
	a->y -= cur_speed;
    }

    trace() << "[ANIMALS] Position updated to: (" << a->x << "," << a->y << ")";

    switch (a->state) {
	case GRAZING:

	    trace() << "[ANIMALS] Currently GRAZING";

	    if (transition < P_GRAZE_WALK) {
		a->state = WALKING;
		trace() << "[ANIMALS] Changed to WALKING";
	    }
	    
	    break;

	case WALKING:

	    trace() << "[ANIMALS] Currently WALKING";

	    if (transition < P_WALK_RUN) {
		a->state = RUNNING;
		trace() << "[ANIMALS] Changed to RUNNING";
	    } else if (transition < P_WALK_GRAZE) {
		a->state = GRAZING;
		trace() << "[ANIMALS] Changed to GRAZING";
	    }

	    break;

	case RUNNING:

	    trace() << "[ANIMALS] Currently RUNNING";

	    if (transition < P_RUN_WALK) {
		a->state = WALKING;
		trace() << "[ANIMALS] Changed to WALKING";
	    }

	    break;

	case THIRSTY:

	    trace() << "[ANIMALS] Currently THIRSTY";

	    break;

	default:
	    a->state = GRAZING;
    } 
}
