#import "MWStartGameState.h"

#import "MWSymb.h"
#import "NSString+Parser.h"
#import "NSArray+Parser.h"

@implementation MWStartGameState

@synthesize field         = _field;
@synthesize users         = _users;
@synthesize symbols       = _symbols;
@synthesize currentPlayer = _currentPlayer;
@synthesize youFirst      = _youFirst;

-(void)dealloc
{
   [ _users         release ];
   [ _symbols       release ];
   [ _currentPlayer release ];

   [ super dealloc ];
}

-(id)initWithDictionary:( NSDictionary* )dict_
{
   self = [ super init ];

   if ( self )
   {
      self.field         = [ [ dict_ objectForKey: @"field" ] integerValue ];
      self.users         = [ [ dict_ objectForKey: @"users" ] arrayOfStringsSeparatedByComma ];
      self.currentPlayer = [ dict_ objectForKey: @"currentPlayer" ];

      self.symbols = [ NSArray arraySymbolsWithDictionary: dict_ ];
   }

   return self;
}

+(id)startGameStateWithDictionary:( NSDictionary* )dict_
{
   return [ [ [ self alloc ] initWithDictionary: dict_ ] autorelease ];
}

-(NSString*)description
{
   return [ NSString stringWithFormat: @"<MWStartGameState field: %d, users: %@, symbs: %@"
           , self.field
           , self.users
           , self.symbols ];
}

@end
