#import "MWCurrentGameState.h"

#import "MWSymb.h"
#import "NSString+Parser.h"
#import "NSArray+Parser.h"

@interface MWCurrentGameState ()

@property ( nonatomic, retain ) NSString* currentPlayer;

@end

@implementation MWCurrentGameState

@synthesize currentPlayer   = _currentPlayer;
@synthesize symbolsAndCoods = _symbolsAndCoods;
@synthesize allPoints       = _allPoints;

-(void)dealloc
{
   [ _symbolsAndCoods release ];
   [ _currentPlayer   release ];
   [ _allPoints       release ];

   [ super dealloc ];
}

-(id)initWithDictionary:( NSDictionary* )dict_
{
    self = [ super init ];

    if ( self )
    {
        self.currentPlayer   = [ dict_ objectForKey: @"currentPlayer" ];
        self.symbolsAndCoods = [ NSArray arraySymbolsAndCoordsWithDictionary: dict_ ];
        NSArray* allPointsStrings_ = [ [ dict_ objectForKey: @"points" ] arrayOfStringsSeparatedByComma ];
        self.allPoints = [ allPointsStrings_ map:(MappingBlock) ^id( NSString* str_ )
        {
            return [ NSNumber numberWithUnsignedInteger: [ str_ integerValue ] ];
        } ];
    }

    return self;
}

+(id)currentGameStateWithDictionary:( NSDictionary* )dict_
{
   return [ [ [ self alloc ] initWithDictionary: dict_ ] autorelease ];
}

-(NSString*)description
{
   return [ NSString stringWithFormat: @"<MWCurrentGameState symbsAndCoords: %@ allPoints: %@>"
           , self.symbolsAndCoods
           , self.allPoints ];
}

@end
