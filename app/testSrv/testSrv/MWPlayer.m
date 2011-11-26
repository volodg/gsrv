#import "MWPlayer.h"

#import <MWSrv/MWSession.h>
#import <MWSrv/MWSymb.h>
#import <MWSrv/MWSymbWithCoords.h>
#import <MWSrv/MWStartGameState.h>

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

-(void)doStepWithSymbsAndCoords:( NSArray* )steps_
{
   [ self.session doStepWithSymbsAndCoords: steps_ ](  nil, nil, ^( id result_, NSError* error_ )
   {
      NSLog( @"doStepWithSymbsAndCoords RESULT: %@ result: %@ error: %@", self.login, result_, error_ );
   } );
}

-(void)start
{
   [ self.session playBattleground ]( nil, nil, ^( id result_, NSError* error_ )
   {
      MWStartGameState* game_state_ = result_;
      MWSymb* game_first_symb_ = [ game_state_.symbols objectAtIndex: 0 ];
      NSLog( @"player: %@ result: %@ error: %@", self.login, result_, error_ );

      MWSymbWithCoords* step_ = [ [ MWSymbWithCoords new ] autorelease ];
      step_.x = 1;
      step_.y = 1;
      step_.symb = game_first_symb_;

      NSArray* steps_ = [ NSArray arrayWithObject: step_ ];
      [ self performSelector: @selector( doStepWithSymbsAndCoords: )
                  withObject: steps_
                  afterDelay: 2.0 ];
   } );
}

@end
