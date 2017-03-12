cplusplus {{
#include "PhysicalProcessMessage_m.h"
#include "DistributedEventDetectionPacket_m.h"
}}

class PhysicalProcessMessage;

struct sourceInfo {
	int ID;
	double x;
	double y;
	double sensedValue;
}

message LocalizationPhysicalProcessMessage extends PhysicalProcessMessage {
	sourceInfo info;
}
