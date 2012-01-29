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

-(id)initWithSymb:( NSString* )symb_ state:( NSUInteger )state_
{
    self = [ super init ];

    if ( self )
    {
        self.symb  = symb_;
        self.state = state_;
    }

    return self;
}

+(id)symbWithSymb:( NSString* )symb_ state:( NSUInteger )state_
{
    return [ [ [ self alloc ] initWithSymb: symb_ state: state_ ] autorelease ];
}

@end
