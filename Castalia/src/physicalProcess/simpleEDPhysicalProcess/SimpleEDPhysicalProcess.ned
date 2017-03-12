package physicalProcess.simpleEDPhysicalProcess;

simple SimpleEDPhysicalProcess extends physicalProcess.customizablePhysicalProcess.CustomizablePhysicalProcess {

	parameters:
		double sensingDistance = default(30);

	@class(SimpleEDPhysicalProcess);

	gates:
		output groundTruthToNode[];
}
