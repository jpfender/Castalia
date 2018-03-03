cplusplus {{
#include "ApplicationPacket_m.h"
}}

class ApplicationPacket;

struct EventInstance {
	int ID;
	simtime_t TS;
	double x;
	double y;
}

struct simpleSourceInfo {
	int ID;
	simtime_t timestamp;
	double x;
	double y;
	bool heard;
}

struct simpleEventReportData {
	unsigned short nodeID;	//the ID of the Node
	simtime_t timestamp;
	double locX;			// x-coordinate of the node
	double locY;			// y-coordinate of the node
} 

packet SimpleEventDetectionDataPacket extends ApplicationPacket {
	simpleEventReportData extraData;
	simpleSourceInfo sources[];
	double sensingDistance;
}

