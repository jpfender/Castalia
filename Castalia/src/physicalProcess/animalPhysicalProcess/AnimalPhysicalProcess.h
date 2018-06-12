#ifndef _ANIMALPHYSICALPROCESS_H_
#define _ANIMALPHYSICALPROCESS_H_

#define ORACLE 70

#define P_GRAZE_WALK    0.5
#define P_WALK_RUN      0.33
#define P_RUN_WALK      0.5
#define P_WALK_GRAZE    0.66

#define GRAZING_SPEED   1.7
#define WALKING_SPEED   7.23
#define RUNNING_SPEED   15.5

#include "CustomizablePhysicalProcess.h"
#include "SimpleEDPhysicalProcessMessage_m.h"

using namespace std;

enum animalState {
    GRAZING,
    WALKING,
    RUNNING,
    THIRSTY
};

typedef struct {
    double x;
    double y;
    double value;
    int state;
    double goal[2];
    bool heard;
    simtime_t when_thirsty;
} animal;

class AnimalPhysicalProcess : public CustomizablePhysicalProcess {
    private:
        sourceOracle *sources;
        int field_x, field_y;
        double sensingDistance; 

        int max_num_animals;
        animal* animals;
        int watering_hole_x, watering_hole_y;

    protected:
        int numAnimals;

        virtual void initialize();
        virtual void handleMessage(cMessage * msg);
        double calculateScenarioReturnValue(const double &x_coo, const
                double &y_coo, const simtime_t & stime);
        bool distanceCutoff(const double &x_coo, const double &y_coo);
        sourceOracle *oracle(const double &x_coo, const double &y_coo, const simtime_t & stime);
        double distToGoal(animal *a);
        void updateAnimalState(animal *a);

};

#endif
