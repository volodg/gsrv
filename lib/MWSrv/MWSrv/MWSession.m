#import "MWSession.h"

#import "MWApi.h"

@interface MWSession ()

@property ( nonatomic, retain ) NSDate* lastLoginDate;
@property ( nonatomic, retain ) MWApi* api;
@property ( nonatomic, retain ) NSString* login;
@property ( nonatomic, retain ) NSString* sid;
@property ( nonatomic, assign ) BOOL gameActive;//???
@property ( nonatomic, retain ) JFFScheduler* scheduler;
@property ( nonatomic, assign ) BOOL buzy;

-(JFFAsyncOperation)privateGetSrvState;
-(void)pingServerState;

@end

@implementation MWSession

@synthesize lastLoginDate = _lastLoginDate;
@synthesize api           = _api;
@synthesize login         = _login;
@synthesize sid           = _sid;
@synthesize handler       = _handler;
@synthesize gameActive    = _gameActive;
@synthesize scheduler     = _scheduler;
@synthesize buzy          = _buzy;

-(void)dealloc
{
   [ _lastLoginDate release ];
   [ _api           release ];
   [ _login         release ];
   [ _sid           release ];
   [ _scheduler     release ];

   [ super dealloc ];
}

-(id)initWithLogin:( NSString* )login_
{
   self = [ super init ];

   if ( self )
   {
      self.login = login_;
      self.api   = [ [ MWApi new ] autorelease ];

      __unsafe_unretained MWSession* self_ = self;
      self.scheduler = [ [ JFFScheduler new ] autorelease ];
      [ self.scheduler addBlock: ^( JFFCancelScheduledBlock cancel_ )
      {
         [ self_ pingServerState ];
      } duration: 5.0 ];
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
      JFFAsyncOperation loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                    , JFFCancelAsyncOperationHandler cancel_callback_
                                    , JFFDidFinishAsyncOperationHandler done_callback_ )
      {
         JFFResultContext* context_ = [ [ JFFResultContext new ] autorelease ];

         JFFAsyncOperation loader_ = [ self.api authWithLogin: self.login sid: nil ];
         loader_ = asyncOperationWithFinishHookBlock( loader_
                                                     , ^( id result_
                                                         , NSError* error_
                                                         , JFFDidFinishAsyncOperationHandler done_callback_ )
         {
            context_.result = result_;
            self.sid = result_;
            done_callback_( result_, error_ );
         } );

         JFFAsyncOperation set_res_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                               , JFFCancelAsyncOperationHandler cancel_callback_
                                               , JFFDidFinishAsyncOperationHandler done_callback_ )
         {
            done_callback_( context_.result, nil );
            return JFFEmptyCancelAsyncOperationBlock;
         };

         loader_ = sequenceOfAsyncOperations( loader_
                                             , [ self privateGetSrvState ]
                                             , set_res_loader_
                                             , nil );

         return loader_( progress_callback_, cancel_callback_, done_callback_ );
      };

      JFFDidFinishAsyncOperationHandler did_finish_operation_ = ^( id result_, NSError* error_ )
      {
         self.lastLoginDate = [ NSDate date ];
      };
      loader_ = [ self asyncOperationForPropertyWithName: @"sid"
                                          asyncOperation: loader_
                                  didFinishLoadDataBlock: did_finish_operation_ ];

      if ( self.lastLoginDate == nil || [ self loginDateExpared ] )
      {
         self.sid = nil;
      }

      return loader_( progress_callback_, cancel_callback_, done_callback_ );
   } copy ] autorelease ];
}

-(JFFAsyncOperation)privateGetSrvState
{
   return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                , JFFCancelAsyncOperationHandler cancel_callback_
                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      NSAssert( self.sid, @"sid can not be empty" );
      JFFAsyncOperation loader_ = [ self.api getSrvStateWithSid: self.sid ];
      return loader_( progress_callback_, cancel_callback_, done_callback_ );
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

   return sequenceOfAsyncOperations( auth_loader_
                                    , cmd_loader_
                                    , [ self privateGetSrvState ]
                                    , nil );
}

-(void)pingServerState
{
   if ( !self.sid || self.buzy )
      return;

   self.buzy = YES;
   JFFAsyncOperation loader_ = [ self.api authWithLogin: self.login sid: self.sid ];
   sequenceOfAsyncOperations( loader_
                             , [ self privateGetSrvState ]
                             , nil )( nil, nil, ^( id result_, NSError* error_ )
   {
      if ( result_ )
      {
         if ( [ result_ isKindOfClass: [ NSArray class ] ]
             && [ result_ count ] > 0 )
         {
            id dict_ = [ result_ objectAtIndex: 0 ];
            id status_ = [ dict_ objectForKey: @"status" ];
            if ( [ status_ integerValue ] != 1 || [ dict_ count ] != 1 )
            {
               NSLog( @"Ping status: %@", status_ );
            }
         }
         NSLog( @"Ping result: %@", result_ );
      }
      self.buzy = NO;
   } );
}

@end
