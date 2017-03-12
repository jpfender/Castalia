#ifndef _SIMPLEEVENTDETECTION_H_
#define _SIMPLEEVENTDETECTION_H_

#include "VirtualApplication.h"
#include "SimpleEventDetectionPacket_m.h"
#include "SimpleEDPhysicalProcess.h"

#include <eigen3/Eigen/Dense>

using namespace std;

typedef Eigen::Vector2d Pos;

static int packetsGenerated = 0;
static int packetsDeliveredToSink = 0;

static int eventsExamined = 0;

static int positives = 0;
static int truePositives = 0;
static int falsePositives = 0;

static int negatives = 0;
static int trueNegatives = 0;
static int falseNegatives = 0;

static float truePositiveRate = 0.0;
static float falsePositiveRate = 0.0;

static float sensitivity = 0.0;
static float specificity = 0.0;

static float precision = 0.0;
static float recall = 0.0;

static vector<EventInstance> groundTruthEvents;
static vector<EventInstance> sinkEvents;

enum SimpleEventDetectionTimers {
	REQUEST_SAMPLE = 1,
	SEND_DATA = 2,
	FLUSH_BUFFER,
};

class SimpleEventDetection: public VirtualApplication {
 private:
	double maxSampleInterval;
	double minSampleInterval;
	double k;
	double a;
	int numSources;
	bool filterOn;
	bool removeOutliers;

	int routingLevel;
	double lastSensedValue;
	int currSentSampleSN;

	double randomBackoffIntervalFraction;
	bool sentOnce;

	double reportThreshold;
	int bufferPeriod;
	int timestampEpsilon;
	bool isBuffering;
	vector<SimpleEventDetectionDataPacket*> buffer;

	simpleSourceInfo *currentOracle;
	double currSensingDistance;

 protected:
	void startup();
	void fromNetworkLayer(ApplicationPacket *, const char *, double, double);
	void handleOracle(SimpleEDPhysicalProcessMessage * oracleMsg);
	void handleMessage(cMessage * msg);
	void handleSensorReading(SensorReadingMessage *);
	void timerFiredCallback(int);
	vector<EventInstance> addToEventInstanceVector(vector<EventInstance> vec, int ID, simtime_t TS);
	void filterAndSend(vector<SimpleEventDetectionDataPacket*> candidates);
	bool multilateration(vector<SimpleEventDetectionDataPacket*> candidates);
	double norm(Pos p);
	void finishSpecific();
};

class PosAndDistance {
	public:
		PosAndDistance() {}
		PosAndDistance(const Pos& pos, double dist)
			: m_pos(pos)
			, m_distance(dist)
		{}
		Pos m_pos;
		double m_distance;
};

typedef vector<PosAndDistance> PosAndDistanceVec;

#endif				// _SIMPLEEVENTDETECTION_APPLICATIONMODULE_H_
