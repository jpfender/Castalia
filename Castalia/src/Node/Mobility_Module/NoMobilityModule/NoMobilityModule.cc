/***************************************************************************
 *  Copyright: National ICT Australia,  2009
 *  Developed at the ATP lab, Networked Systems theme
 *  Author(s): Yuri Tselishchev
 *  This file is distributed under the terms in the attached LICENSE file.
 *  If you do not find this file, copies can be found by writing to:
 *
 *      NICTA, Locked Bag 9013, Alexandria, NSW 1435, Australia
 *      Attention:  License Inquiry.
 ***************************************************************************/

#include "NoMobilityModule.h"

Define_Module(NoMobilityModule);

void NoMobilityModule::handleMessage(cMessage * msg)
{
	int msgKind = msg->getKind();
	trace() << "Unexpected message: " << msgKind;
	delete msg;
	msg = NULL;
}
