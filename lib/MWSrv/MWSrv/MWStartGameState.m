#import "MWStartGameState.h"

#import "MWSymb.h"

@interface NSString (MWStartGameState)
@end

@implementation NSString (MWStartGameState)

-(NSArray*)arrayOfStringsSeparatedByComma
{
   NSArray* result_ = [ self componentsSeparatedByString: @"," ];
   return [ result_ select: ^BOOL( id chunk_ )
   {
      return [ chunk_ length ] > 0;
   } ];
}

@end

@implementation MWStartGameState

@synthesize field   = _field;
@synthesize users   = _users;
@synthesize symbols = _symbols;

-(void)dealloc
{
   [ _users   release ];
   [ _symbols release ];

   [ super dealloc ];
}

-(id)initWithDictionary:( NSDictionary* )dict_
{
   self = [ super init ];

   if ( self )
   {
      self.field = [ [ dict_ objectForKey: @"field" ] integerValue ];
      self.users = [ [ dict_ objectForKey: @"users" ] arrayOfStringsSeparatedByComma ];

      NSArray* letters_ = [ [ dict_ objectForKey: @"sym"   ] arrayOfStringsSeparatedByComma ];
      NSArray* states_  = [ [ dict_ objectForKey: @"state" ] arrayOfStringsSeparatedByComma ];

      NSMutableArray* symbols_ = [ NSMutableArray arrayWithCapacity: [ states_ count ] ];
      [ letters_ transformWithArray: states_
                          withBlock: ^void( id first_object_, id second_object_ )
      {
         MWSymb* symb_ = [ [ MWSymb new ] autorelease ];
         symb_.symb  = first_object_;
         symb_.state = [ second_object_ integerValue ];
         [ symbols_ addObject: symb_ ];
      } ];
      self.symbols = [ NSArray arrayWithArray: symbols_ ];
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
