#import "MWPlayer.h"

#import <MWSrv/MWSession.h>
#import <MWSrv/MWSymb.h>
#import <MWSrv/MWSymbWithCoords.h>
#import <MWSrv/MWStartGameState.h>

@interface MWPlayer ()

@property ( nonatomic, retain ) NSString* login;
@property ( nonatomic, retain ) MWSession* session;
@property ( nonatomic, retain ) MWStartGameState* gameState;
@property ( nonatomic, assign ) BOOL justWait;

@end

@implementation MWPlayer

@synthesize session   = _session;
@synthesize login     = _login;
@synthesize gameState = _gameState;
@synthesize justWait  = _justWait;

-(void)dealloc
{
   [ _session   release ];
   [ _login     release ];
   [ _gameState release ];

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

-(void)doStepWithSymb:( NSString* )symb_
{
   if ( !symb_ )
   {
      //GTODO assert
      return;
   }

   MWSymbWithCoords* step_ = [ [ MWSymbWithCoords new ] autorelease ];
   step_.x = 1;
   step_.y = 1;

   MWSymb* smartSymb_ = [ [ MWSymb new ] autorelease ];
   smartSymb_.symb = symb_;
   smartSymb_.state = 0;
   step_.symb = smartSymb_;
   NSArray* steps_ = [ NSArray arrayWithObject: step_ ];
//   [ self.session doStepWithSymbsAndCoords: steps_ scores: 25 + rand()%4 ](  nil, nil, ^( id result_, NSError* error_ )
//   {
//      NSLog( @"doStep RESULT: %@ result: %@ error: %@", self.login, result_, error_ );
//   } );
}

-(void)start
{
   [ self.session playBattleground ]( nil, nil, ^( id result_, NSError* error_ )
   {
      NSLog( @"didStart player: %@ state: %@ error: %@", self.login, result_, error_ );
      self.gameState = result_;

      if ( !self.gameState.youFirst && !error_ )
      {
//          NSLog( @"start wait srv state" );
         [ self wait ];
      }
   } );
}

-(void)wait
{
//   [ self.session getSymbolsCount: 7 ](  nil, nil, ^( id result_, NSError* error_ )
//   {
//      NSLog( @"getSymbolsCount RESULT: %@ result: %@ error: %@", self.login, result_, error_ );
//   } );
}

-(void)skipStep
{
   [ self.session skipStep ](  nil, nil, ^( id result_, NSError* error_ )
   {
      NSLog( @"skipStep RESULT: %@ result: %@ error: %@", self.login, result_, error_ );
   } );
}

@end
