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

-(void)dealloc
{
   [ _symbolsAndCoods release ];
   [ _currentPlayer   release ];

   [ super dealloc ];
}

-(id)initWithDictionary:( NSDictionary* )dict_
{
   self = [ super init ];

   if ( self )
   {
      self.currentPlayer = [ dict_ objectForKey: @"currentPlayer" ];
      self.symbolsAndCoods = [ NSArray arraySymbolsAndCoordsWithDictionary: dict_ ];
   }

   return self;
}

+(id)currentGameStateWithDictionary:( NSDictionary* )dict_
{
   return [ [ [ self alloc ] initWithDictionary: dict_ ] autorelease ];
}

-(NSString*)description
{
   return [ NSString stringWithFormat: @"<MWCurrentGameState symbsAndCoords: %@"
           , self.symbolsAndCoods ];
}

@end
