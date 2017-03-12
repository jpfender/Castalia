#ifndef _SIMPLEEDPHYSICALPROCESS_H_
#define _SIMPLEEDPHYSICALPROCESS_H_

#define ORACLE 70

#include "CustomizablePhysicalProcess.h"
#include "SimpleEDPhysicalProcessMessage_m.h"

using namespace std;

class SimpleEDPhysicalProcess : public CustomizablePhysicalProcess {
    private:
        sourceOracle *sources;
        double sensingDistance; 

    protected:
        virtual void initialize();
        virtual void handleMessage(cMessage * msg);
        bool distanceCutoff(const double &x_coo, const double &y_coo);
        sourceOracle *oracle(const double &x_coo, const double &y_coo, const simtime_t & stime);

};

#endif
