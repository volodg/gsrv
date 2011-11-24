#import "MWSession.h"

#import "MWApi.h"

@interface MWSession ()

@property ( nonatomic, retain ) NSDate* lastLoginDate;
@property ( nonatomic, retain ) MWApi* api;
@property ( nonatomic, retain ) NSString* login;
@property ( nonatomic, retain ) NSString* sid;
@property ( nonatomic, retain ) JFFScheduler* scheduler;
@property ( nonatomic, assign ) BOOL buzy;
@property ( nonatomic, assign ) BOOL stateMayChanged;

-(void)pingServerState;

@end

@implementation MWSession

@synthesize lastLoginDate   = _lastLoginDate;
@synthesize api             = _api;
@synthesize login           = _login;
@synthesize sid             = _sid;
@synthesize handler         = _handler;
@synthesize scheduler       = _scheduler;
@synthesize buzy            = _buzy;
@synthesize stateMayChanged = _stateMayChanged;

-(void)dealloc
{
   [ _lastLoginDate release ];
   [ _api release ];
   [ _login release ];

   [ super dealloc ];
}

-(id)initWithLogin:( NSString* )login_
{
   self = [ super init ];

   if ( self )
   {
      self.login = login_;
      self.api = [ [ MWApi new ] autorelease ];

      self.scheduler = [ [ JFFScheduler new ] autorelease ];

      __unsafe_unretained MWSession* self_ = self;
      [ self.scheduler addBlock: ^( JFFCancelScheduledBlock cancel_ )
      {
         [ self_ pingServerState ];
      } duration: 3. ];
   }

   return self;
}

+(id)sessionWithLogin:( NSString* )login_
{
   return [ [ [ self alloc ] initWithLogin: login_ ] autorelease ];
}

-(BOOL)loginDateExpared
{
   NSTimeInterval interval_ = [ self.lastLoginDate timeIntervalSinceNow ];
   return interval_ < -4.8 * 60;
}

-(JFFAsyncOperation)authLoader
{
   return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                , JFFCancelAsyncOperationHandler cancel_callback_
                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      JFFAsyncOperation loader_ = [ self.api authWithLogin: self.login ];

      JFFDidFinishAsyncOperationHandler did_finish_operation_ = ^( id result_, NSError* error_ )
      {
         self.lastLoginDate = [ NSDate date ];
      };
      loader_ = [ self asyncOperationForPropertyWithName: @"sid"
                                          asyncOperation: loader_
                                  didFinishLoadDataBlock: did_finish_operation_ ];
      if ( self.lastLoginDate == nil ||
          [ self loginDateExpared ] )
      {
         self.sid = nil;
      }
      return loader_( progress_callback_, cancel_callback_, done_callback_ );
   } copy ] autorelease ];
}

-(JFFAsyncOperation)privateGetSrvState
{
   JFFAsyncOperation auth_loader_ = [ self authLoader ];
   JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                     , JFFCancelAsyncOperationHandler cancel_callback_
                                     , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      return [ self.api getSrvStateWithSid: self.sid ]( progress_callback_
                                                       , cancel_callback_
                                                       , done_callback_ );
   };

   return sequenceOfAsyncOperations( auth_loader_, cmd_loader_, nil );
}

-(JFFAsyncOperation)updateStateMayChanged
{
   return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                , JFFCancelAsyncOperationHandler cancel_callback_
                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      self.stateMayChanged = YES;
      if ( done_callback_ )
         done_callback_( [ NSNull null ], nil );
      return JFFEmptyCancelAsyncOperationBlock;
   } copy ] autorelease ];
}

-(JFFAsyncOperation)playBattleground
{
   JFFAsyncOperation auth_loader_ = [ self authLoader ];
   JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                     , JFFCancelAsyncOperationHandler cancel_callback_
                                     , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      return [ self.api playBattlegroundForSid: self.sid ]( progress_callback_
                                                           , cancel_callback_
                                                           , done_callback_ );
   };

   //GTODO put in load balancer
   return sequenceOfAsyncOperations( auth_loader_
                                    , cmd_loader_
                                    , [ self updateStateMayChanged ]
                                    , nil );
}

//GTODO put in load balancer
-(void)pingServerState
{
   if ( !self.handler || self.buzy || !self.stateMayChanged )
      return;

   self.buzy = YES;
   self.stateMayChanged = NO;
   [ self privateGetSrvState ]( nil, nil, ^( id result_, NSError* error_ )
   {
      self.buzy = NO;
      if ( result_ && self.handler )
         self.handler( result_ );
   } );
}

@end
