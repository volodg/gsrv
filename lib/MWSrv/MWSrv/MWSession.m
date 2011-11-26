#import "MWSession.h"

#import "MWApi.h"
#import "MWStartGameState.h"

#import "NSObject+Parser.h"

@interface MWSession ()

@property ( nonatomic, retain ) NSDate* lastLoginDate;
@property ( nonatomic, retain ) MWApi* api;
@property ( nonatomic, retain ) NSString* login;
@property ( nonatomic, retain ) NSString* sid;
@property ( nonatomic, assign ) BOOL gameActive;//???
@property ( nonatomic, assign ) BOOL buzy;

-(JFFAsyncOperation)privateGetSrvState;

@end

@implementation MWSession

@synthesize lastLoginDate = _lastLoginDate;
@synthesize api           = _api;
@synthesize login         = _login;
@synthesize sid           = _sid;
@synthesize handler       = _handler;
@synthesize gameActive    = _gameActive;
@synthesize buzy          = _buzy;

-(void)dealloc
{
   [ _lastLoginDate release ];
   [ _api           release ];
   [ _login         release ];
   [ _sid           release ];

   [ super dealloc ];
}

-(id)initWithLogin:( NSString* )login_
{
   self = [ super init ];

   if ( self )
   {
      self.login = login_;
      self.api   = [ [ MWApi new ] autorelease ];
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

//GTODO limit repeat count
-(JFFAsyncOperation)getGameStarted
{
   PredicateBlock predicate_ = ^BOOL( id context_ )
   {
      return ![ [ context_ result ] isGameStartedResponse ];
   };

   JFFAsyncOperation loader_ = repeatAsyncOperation( [ self privateGetSrvState ]
                                                    , predicate_
                                                    , 1.
                                                    , 60 );

   return asyncOperationWithFinishHookBlock( loader_
                                            , ^( id result_
                                                , NSError* error_
                                                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      result_ = [ result_ firstMatch: ^BOOL( id object_ )
      {
         return [ object_ isStartGameStateResponse ];
      } ];

      result_ = result_ ? [ MWStartGameState startGameStateWithDictionary: result_ ] : nil;
      done_callback_( result_, error_ );
   } );
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

   //STODO place 
   return sequenceOfAsyncOperations( auth_loader_
                                    , cmd_loader_
                                    , [ self privateGetSrvState ]
                                    , [ self getGameStarted ]
                                    , nil );
}

-(void)exitGame
{
   self.sid = nil;
}

@end
