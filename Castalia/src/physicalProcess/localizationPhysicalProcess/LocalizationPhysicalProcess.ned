package physicalProcess.localizationPhysicalProcess;

simple LocalizationPhysicalProcess extends physicalProcess.customizablePhysicalProcess.CustomizablePhysicalProcess {

	@class(LocalizationPhysicalProcess);

	description = default ("Extends customizablePhysicalProcess to \
	return ID and location of strongest physical process source along \
	with sensor readings");

	gates:
		output groundTruthToNode[];
}
