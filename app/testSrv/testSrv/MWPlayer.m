#import "MWPlayer.h"

#import <MWSrv/MWSession.h>

@interface MWPlayer ()

@property ( nonatomic, retain ) NSString* login;
@property ( nonatomic, retain ) MWSession* session;

@end

@implementation MWPlayer

@synthesize session = _session;
@synthesize login   = _login;

-(void)dealloc
{
   [ _session release ];
   [ _login   release ];

   [ super dealloc ];
}

-(id)initWithLogin:( NSString* )login_
{
   self = [ super init ];

   if ( self )
   {
      self.login   = login_;
      self.session = [ MWSession sessionWithLogin: login_ ];
   }

   return self;
}

+(id)playerWithLogin:( NSString* )login_
{
   return [ [ [ self alloc ] initWithLogin: login_ ] autorelease ];
}

-(void)start
{
   [ self.session playBattleground ]( nil, nil, ^( id result_, NSError* error_ )
   {
      NSLog( @"player: %@ result: %@ error: %@", self.login, result_, error_ );
      [ self.session getSymbolsCount: 5 ](  nil, nil, ^( id result_, NSError* error_ )
      {
         NSLog( @"getSymbolsCount: %@ result: %@ error: %@", self.login, result_, error_ );
      } );
   } );
}

@end
