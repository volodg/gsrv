#import "MWSession.h"

#import "MWApi.h"
#import "MWStartGameState.h"
#import "MWCurrentGameState.h"

#import "NSObject+Parser.h"
#import "NSArray+Parser.h"

@interface MWSessionState : NSObject

@property ( nonatomic, retain ) NSString* sid;
@property ( nonatomic, retain ) MWStartGameState* startGameState;
@property ( nonatomic, assign ) NSUInteger lastStepPoints;

@end

@implementation MWSessionState

@synthesize sid            = _sid;
@synthesize startGameState = _startGameStatel;
@synthesize lastStepPoints = _lastStepPoints;

-(void)dealloc
{
   [ _sid             release ];
   [ _startGameStatel release ];

   [ super dealloc ];
}

@end

@interface MWSession ()

@property ( nonatomic, retain ) NSDate* lastLoginDate;//GTODO 5 minutes after last request
@property ( nonatomic, retain ) MWApi* api;
@property ( nonatomic, retain ) NSString* login;
@property ( nonatomic, retain ) MWSessionState* sessionState;
@property ( nonatomic, retain ) MWCurrentGameState* currentGameState;

-(JFFAsyncOperation)privateGetSrvState;
-(JFFAsyncOperation)privateExitGame;

@end

@implementation MWSession

@synthesize lastLoginDate = _lastLoginDate;
@synthesize api           = _api;
@synthesize login         = _login;
@synthesize sessionState  = _sessionState;
@synthesize currentGameState = _currentGameState;

-(void)dealloc
{
   [ _lastLoginDate release ];
   [ _api           release ];
   [ _login         release ];
   [ _sessionState  release ];

   [ super dealloc ];
}

-(id)initWithLogin:( NSString* )login_
{
   self = [ super init ];

   if ( self )
   {
      self.login        = login_;
      self.api          = [ [ MWApi new ] autorelease ];
   }

   return self;
}

+(id)sessionWithLogin:( NSString* )login_
{
   return [ [ [ self alloc ] initWithLogin: login_ ] autorelease ];
}

-(MWSessionState*)sessionState
{
   if ( !_sessionState )
   {
      _sessionState = [ MWSessionState new ];
   }
   return _sessionState;
}

-(BOOL)loginDateExpared
{
   NSTimeInterval interval_ = [ self.lastLoginDate timeIntervalSinceNow ];
   return interval_ < -4.8 * 60;
}

//GTODO refactor
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
            self.sessionState.sid = result_;
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
      loader_ = [ self.sessionState asyncOperationForPropertyWithName: @"sid"
                                                       asyncOperation: loader_
                                               didFinishLoadDataBlock: did_finish_operation_ ];

      if ( self.lastLoginDate == nil || [ self loginDateExpared ] )
      {
         //GTODO remove this
         self.sessionState.sid = nil;
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
      NSAssert( _sessionState.sid, @"sid can not be empty" );
      JFFAsyncOperation loader_ = [ self.api getSrvStateWithSid: self.sessionState.sid ];
      done_callback_ = [ [ done_callback_ copy ] autorelease ];
      return loader_( progress_callback_, cancel_callback_, ^( id result_, NSError* error_ )
      {
         if ( done_callback_ )
            done_callback_( result_, error_ );
      } );
   } copy ] autorelease ];
}

//GTODO limit repeat count
-(JFFAsyncOperation)getGameStarted
{
   return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                , JFFCancelAsyncOperationHandler cancel_callback_
                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      JFFAsyncOperation loader_ = [ self privateGetSrvState ];

//      PredicateBlock predicate_ = ^BOOL( JFFResultContext* object_ )
//      {
//         BOOL result_ = ![ object_.result isKindOfClass: [ NSArray class ] ]
//         || [ object_.result firstMatch: ^BOOL( id object_ )
//         {
//            return [ object_ isGameStartedResponse ];
//         } ] == nil;
//         return result_;
//      };
//      loader_ = repeatAsyncOperation( loader_
//                                     , predicate_
//                                     , 1.
//                                     , 60 );

      loader_ = asyncOperationWithFinishHookBlock( loader_
                                               , ^( id result_
                                                   , NSError* error_
                                                   , JFFDidFinishAsyncOperationHandler done_callback_ )
      {
         //NSLog( @"done getGameStarted!!!: %@ error: %@", result_, error_ );
         //GTODO check response on NSNull
         result_ = [ result_ firstMatch: ^BOOL( id object_ )
         {
            return [ object_ isGameStartedResponse ];
         } ];

         if ( result_ )
         {
            MWStartGameState* state_ = [ MWStartGameState startGameStateWithDictionary: result_ ];
            state_.youFirst = [ self.login isEqualToString: state_.currentPlayer ];
            self.sessionState.startGameState = state_;

            result_ = state_;
         }

         done_callback_( result_, error_ );
      } );
      return loader_( progress_callback_
                     , cancel_callback_
                     , done_callback_ );
   } copy ] autorelease ];
}

-(JFFAsyncOperation)playBattleground
{
   JFFAsyncOperation auth_loader_ = [ self authLoader ];
   JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                     , JFFCancelAsyncOperationHandler cancel_callback_
                                     , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      return [ self.api playBattlegroundForSid: self.sessionState.sid ]( progress_callback_
                                                                        , cancel_callback_
                                                                        , done_callback_ );
   };

   //STODO place 
   return sequenceOfAsyncOperations( auth_loader_
                                    , [ self privateExitGame ]
                                    , cmd_loader_
                                    , [ self privateGetSrvState ]
                                    , [ self getGameStarted ]
                                    , nil );
}

-(JFFAsyncOperation)privateGetSymbolsSrvState
{
   JFFAsyncOperation loader_ = [ self privateGetSrvState ];

   return asyncOperationWithFinishHookBlock( loader_
                                            , ^( id result_
                                                , NSError* error_
                                                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      result_ = [ result_ firstMatch: ^BOOL( id object_ )
      {
         return [ object_ isGetSymbolsResponse ];
      } ];

      result_ = result_ ? [ NSArray arraySymbolsWithDictionary: result_ ] : nil;
      done_callback_( result_, error_ );
   } );
}

-(JFFAsyncOperation)getSymbolsCount:( NSUInteger )count_
{
   JFFAsyncOperation auth_loader_ = [ self authLoader ];
   JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                     , JFFCancelAsyncOperationHandler cancel_callback_
                                     , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      return [ self.api getSymbolsWithSid: self.sessionState.sid
                                    count: count_ ]( progress_callback_
                                                    , cancel_callback_
                                                    , done_callback_ );
   };

   //STODO place in load balancer
   //STODO add start game in sequence
   return sequenceOfAsyncOperations( auth_loader_
                                    , cmd_loader_
                                    , [ self privateGetSymbolsSrvState ]
                                    , nil );
}

-(JFFAsyncOperation)privateGetNextStepSrvState
{
   return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                , JFFCancelAsyncOperationHandler cancel_callback_
                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      JFFAsyncOperation loader_ = [ self privateGetSrvState ];

      loader_ = asyncOperationWithFinishHookBlock( loader_
                                                  , ^( id result_
                                                      , NSError* error_
                                                      , JFFDidFinishAsyncOperationHandler lc_done_callback_ )
      {
         NSLog( @"privateGetNextStepSrvState SRV: %@", result_ );
         if ( [ result_ isKindOfClass: [ NSNull class ] ] )
         {
            //GTODO finish game here
            done_callback_( nil, [ JFFError errorWithDescription: @"wait timeout" ] );
            return;
         }
         result_ = [ result_ firstMatch: ^BOOL( id object_ )
         {
            return [ object_ isCurrentGameSateResponse ];
         } ];

         if ( result_ )
         {
            //self.sessionState.startGameState
            NSArray* cids_ = self.sessionState.startGameState.users;
            MWCurrentGameState* currState_ = [ MWCurrentGameState currentGameStateWithDictionary: result_
                                                                                         userCid: self.login//GTODO change login on cid here
                                                                                       usersCids: cids_ ];
            self.currentGameState = currState_;

            result_ = currState_;
         }

         lc_done_callback_( result_, error_ );
      } );

      return loader_( progress_callback_, cancel_callback_, done_callback_ );
   } copy ] autorelease ];
}

-(JFFAsyncOperation)privateExitGame
{
   JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                     , JFFCancelAsyncOperationHandler cancel_callback_
                                     , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      return [ self.api exitGameWithSid: self.sessionState.sid ]( progress_callback_
                                                                 , cancel_callback_
                                                                 , done_callback_ );
   };
   return sequenceOfAsyncOperations( cmd_loader_
                                    , [ self privateGetSrvState ]
                                    , nil );
}

-(JFFAsyncOperation)doStepWithSymbsAndCoords:( NSArray* )step_
                                      scores:( NSUInteger )points_
{
   JFFAsyncOperation auth_loader_ = [ self authLoader ];

   JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                     , JFFCancelAsyncOperationHandler cancel_callback_
                                     , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      self.sessionState.lastStepPoints = points_;
      return [ self.api doStepWithSid: self.sessionState.sid
                       symbsAndCoords: step_
                               points: points_ ]( progress_callback_
                                                 , cancel_callback_
                                                 , done_callback_ );
   };

   //STODO place in load balancer
   //STODO add start game in sequence
   return sequenceOfAsyncOperations( auth_loader_
                                    , cmd_loader_
                                    , [ self privateGetNextStepSrvState ]
                                    , nil );
}

-(JFFAsyncOperation)skipStep
{
   JFFAsyncOperation auth_loader_ = [ self authLoader ];

   JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                     , JFFCancelAsyncOperationHandler cancel_callback_
                                     , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      JFFAsyncOperation loader_ = [ self.api doStepWithSid: self.sessionState.sid
                                            symbsAndCoords: nil
                                                    points: self.sessionState.lastStepPoints ];
      return loader_( progress_callback_
                     , cancel_callback_
                     , done_callback_ );
   };
   cmd_loader_ = sequenceOfAsyncOperations( cmd_loader_
                                           , [ self privateGetNextStepSrvState ]
                                           , nil );

   //STODO place in load balancer
   return sequenceOfAsyncOperations( auth_loader_, cmd_loader_, nil );
}

-(JFFAsyncOperation)waitStep
{
   return [ self privateGetNextStepSrvState ];
}

-(void)exitGame
{
   if ( _sessionState.sid )
      [ self.api exitGameWithSid: self.sessionState.sid ]( nil, nil, nil );
   self.sessionState = nil;
}

@end
