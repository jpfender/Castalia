package node.application.simpleEventDetection;

// The sensor node module. Connects to the wireless channel in order to communicate
// with other nodes. Connects to psysical processes so it can sample them.

simple SimpleEventDetection like node.application.iApplication {
 parameters:
 	string applicationID = default ("simpleEventDetection");
	bool collectTraceInfo = default (true);
	int priority = default (1);
	int packetHeaderOverhead = default (8);	// in bytes
	int constantDataPayload = default (12);	// in bytes
	int maxSampleInterval = default (5000);// in msec
	int minSampleInterval = default (1000);	// in msec
	double multiplicative_k = default (0.25);	// multiplicative parameter (k)
	double attenuation_exp_a = default (1.0);	// attenuation exponent (a)
	bool isSink = default (false);
	bool filterOn = default (true);
	bool removeOutliers = default (false);

	int numSources = default(1);
	double reportThreshold = default(1);
	int bufferPeriod = default(10000); // how long to hold received packets in buffer (msec)
	int timestampEpsilon = default(1000); // time window for events to be treated as simultaneous (msec)

 gates:
 	output toCommunicationModule;
	output toSensorDeviceManager;
	input fromCommunicationModule;
	input fromSensorDeviceManager;
	input fromResourceManager;

	input groundTruthFromNodeContainerModule[];

}

