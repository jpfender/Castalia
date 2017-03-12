#ifndef _LOCALIZATIONPHYSICALPROCESS_H_
#define _LOCALIZATIONPHYSICALPROCESS_H_

#define GROUND_TRUTH 60

#include "CustomizablePhysicalProcess.h"
#include "LocalizationPhysicalProcessMessage_m.h"

using namespace std;

class LocalizationPhysicalProcess : public CustomizablePhysicalProcess {
    protected:
        virtual void handleMessage(cMessage * msg);
        sourceInfo getStrongestSource(const double &x_coo, const double &y_coo);

};

#endif
