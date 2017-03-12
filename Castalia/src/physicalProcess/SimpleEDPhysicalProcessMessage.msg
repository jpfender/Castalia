cplusplus {{
#include "PhysicalProcessMessage_m.h"
#include "SimpleEventDetectionPacket_m.h"
}}

class PhysicalProcessMessage;

struct sourceOracle {
	int ID;
	simtime_t timestamp;
	double x;
	double y;
	bool heard;
}

message SimpleEDPhysicalProcessMessage extends PhysicalProcessMessage {
	sourceOracle sources[];
	double sensingDistance;
}
