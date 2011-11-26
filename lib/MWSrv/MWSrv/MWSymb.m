#import "MWSymb.h"

@implementation MWSymb

@synthesize symb  = _symb;
@synthesize state = _state;

-(NSString*)description
{
   return [ NSString stringWithFormat: @"<MWSymb symb: %@, state: %d"
           , self.symb
           , self.state ];
}

@end
