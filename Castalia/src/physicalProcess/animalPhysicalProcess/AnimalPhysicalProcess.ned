package physicalProcess.animalPhysicalProcess;

simple AnimalPhysicalProcess extends physicalProcess.customizablePhysicalProcess.CustomizablePhysicalProcess {

	parameters:
		collectTraceInfo = false;
		int field_x = default(100);
		int field_y = default(100);
		double sensingDistance = default(30);

		int numAnimals = default(5);

	@class(AnimalPhysicalProcess);

	gates:
		output groundTruthToNode[];
}
