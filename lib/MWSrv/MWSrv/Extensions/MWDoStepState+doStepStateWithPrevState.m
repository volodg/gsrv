#import "MWDoStepState+doStepStateWithPrevState.h"

@implementation MWDoStepState (doStepStateWithPrevState)

-(id)doStepStateWithPrevState:( MWDoStepState* )prevState_
{
    MWDoStepState* result_ = [ MWDoStepState new ];

    result_.points   = self.points;
    result_.tperturn = self.tperturn;

    result_.stused  = self.stused  - prevState_.stused;
    result_.mnused  = self.mnused  - prevState_.mnused;
    result_.jadvise = self.jadvise - prevState_.jadvise;
    result_.sp1     = self.sp1     - prevState_.sp1;
    result_.sp2     = self.sp2     - prevState_.sp2;
    result_.sp3     = self.sp3     - prevState_.sp3;
    result_.sp4     = self.sp4     - prevState_.sp4;
    result_.elem1   = self.elem1   - prevState_.elem1;
    result_.elem2   = self.elem2   - prevState_.elem2;
    result_.elem3   = self.elem3   - prevState_.elem3;
    result_.elem4   = self.elem4   - prevState_.elem4;
    result_.bnst1   = self.bnst1   - prevState_.bnst1;
    result_.bnst2   = self.bnst2   - prevState_.bnst2;
    result_.bnst3   = self.bnst3   - prevState_.bnst3;
    result_.bnst4   = self.bnst4   - prevState_.bnst4;

    return [ result_ autorelease ];
}

@end
