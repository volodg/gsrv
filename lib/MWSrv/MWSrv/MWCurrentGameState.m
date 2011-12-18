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
@synthesize ownPoints       = _ownPoints;
@synthesize opponentPoints  = _opponentPoints;
@synthesize opponentName    = _opponentName;

-(void)dealloc
{
   [ _symbolsAndCoods release ];
   [ _currentPlayer   release ];
   [ _allPoints       release ];
   [ _opponentName    release ];

   [ super dealloc ];
}

-(id)initWithDictionary:( NSDictionary* )dict_
                userCid:( NSString* )cid_
              usersCids:( NSArray* )cids_
{
   self = [ super init ];

   if ( self )
   {
      self.currentPlayer   = [ dict_ objectForKey: @"currentPlayer" ];
      self.symbolsAndCoods = [ NSArray arraySymbolsAndCoordsWithDictionary: dict_ ];
      NSArray* allPointsStrings_ = [ [ dict_ objectForKey: @"points" ] arrayOfStringsSeparatedByComma ];
      self.allPoints = [ allPointsStrings_ map: ^id( id str_ )
      {
         return [ NSNumber numberWithUnsignedInteger: [ str_ integerValue ] ];
      } ];

      NSUInteger ownIndex_      = [ cids_ indexOfObject: cid_ ];
      NSUInteger opponentIndex_ = 1 - ownIndex_;

      self.ownPoints      = [ [ self.allPoints objectAtIndex: ownIndex_      ] integerValue ];
      self.opponentPoints = [ [ self.allPoints objectAtIndex: opponentIndex_ ] integerValue ];

      //GTODO ose names instead of cids
      self.opponentName = [ cids_ objectAtIndex: opponentIndex_ ];
   }

   return self;
}

+(id)currentGameStateWithDictionary:( NSDictionary* )dict_
                            userCid:( NSString* )cid_
                          usersCids:( NSArray* )cids_
{
   return [ [ [ self alloc ] initWithDictionary: dict_
                                        userCid: cid_
                                      usersCids: cids_ ] autorelease ];
}

-(NSString*)description
{
   return [ NSString stringWithFormat: @"<MWCurrentGameState symbsAndCoords: %@ allPoints: %@ ownPoints: %d opponentPoints: %d"
           , self.symbolsAndCoods
           , self.allPoints
           , self.ownPoints
           , self.opponentPoints ];
}

@end
