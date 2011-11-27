#import "MWSymbWithCoords.h"

@implementation MWSymbWithCoords

@synthesize symb = _symb;
@synthesize x    = _x;
@synthesize y    = _y;

-(void)dealloc
{
   [ _symb release ];

   [ super dealloc ];
}

-(NSString*)description
{
   return [ NSString stringWithFormat: @"<MWSymbWithCoords symb: %@, x: %d, y: %d"
           , self.symb
           , self.x
           , self.y ];
}

@end
