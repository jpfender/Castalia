#ifndef _SIMPLEEVENTDETECTION_H_
#define _SIMPLEEVENTDETECTION_H_

#include "VirtualApplication.h"
#include "SimpleEventDetectionPacket_m.h"
#include "SimpleEDPhysicalProcess.h"

#include <eigen3/Eigen/Dense>

using namespace std;

typedef Eigen::Vector2d Pos;

vector<EventInstance> groundTruthEvents;
vector<EventInstance> sinkEvents;

enum SimpleEventDetectionTimers {
	REQUEST_SAMPLE = 1,
	SEND_DATA      = 2,
	FLUSH_BUFFER   = 3,
	FLUSH_OUTLIERS = 4
};

class SimpleEventDetection: public VirtualApplication {
 private:
	double maxSampleInterval;
	double minSampleInterval;
	double k;
	double a;
	int numSources;
	bool filterOn;
	bool filterIdeal;
	bool removeOutliers;
	bool bufferOutliers;

	int routingLevel;
	double lastSensedValue;
	int currSentSampleSN;

	double randomBackoffIntervalFraction;
	bool sentOnce;

	double reportThreshold;
	int bufferPeriod;
	int bufferThreshold;
	int timestampEpsilon;
	bool isBuffering;
	bool outliersBuffering;
	vector<SimpleEventDetectionDataPacket*> buffer;
	vector<SimpleEventDetectionDataPacket*> outliers;

	simpleSourceInfo *currentOracle;
	double currSensingDistance;

 protected:
	void startup();
	void fromNetworkLayer(ApplicationPacket *, const char *, double, double);
	void handleOracle(SimpleEDPhysicalProcessMessage * oracleMsg);
	void handleMessage(cMessage * msg);
	void handleSensorReading(SensorReadingMessage *);
	void timerFiredCallback(int);
	void flushBuffer();
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
