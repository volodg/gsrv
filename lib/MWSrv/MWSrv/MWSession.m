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
@property ( nonatomic, assign ) BOOL active;

@end

@implementation MWSessionState

@synthesize sid            = _sid;
@synthesize startGameState = _startGameStatel;
@synthesize lastStepPoints = _lastStepPoints;
@synthesize active         = _active;

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
@property ( nonatomic, copy ) JFFDidFinishAsyncOperationHandler startGameCallback;
@property ( nonatomic, copy ) JFFDidFinishAsyncOperationHandler stateGameCallback;
@property ( nonatomic, copy ) JFFDidFinishAsyncOperationHandler symbolsCallback;

-(JFFAsyncOperation)privateExitGame;

@end

@implementation MWSession

@synthesize lastLoginDate     = _lastLoginDate;
@synthesize api               = _api;
@synthesize login             = _login;
@synthesize sessionState      = _sessionState;
@synthesize currentGameState  = _currentGameState;
@synthesize startGameCallback = _startGameCallback;
@synthesize stateGameCallback = _stateGameCallback;
@synthesize symbolsCallback   = _symbolsCallback;

-(void)dealloc
{
    [ _lastLoginDate     release ];
    [ _api               release ];
    [ _login             release ];
    [ _sessionState      release ];
    [ _startGameCallback release ];
    [ _stateGameCallback release ];
    [ _symbolsCallback   release ];

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
   //NSTimeInterval interval_ = [ self.lastLoginDate timeIntervalSinceNow ];
   return NO;//interval_ < -4.8 * 60;
}

-(JFFAsyncOperation)privateGetSrvState2
{
    return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                 , JFFCancelAsyncOperationHandler cancel_callback_
                 , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        NSAssert( _sessionState.sid, @"sid can not be empty" );
        JFFAsyncOperation loader_ = [ self.api getSrvStateWithSid: self.sessionState.sid ];
        done_callback_ = [ [ done_callback_ copy ] autorelease ];
        return loader_( progress_callback_, cancel_callback_, ^void( id result_, NSError* error_ )
        {
            if ( done_callback_ )
                done_callback_( result_, error_ );
        } );
    } copy ] autorelease ];
}

-(void)processServerResponse:( NSArray* )responses_
{
    NSLog( @"processServerResponse user: %@ data: %@", self.login, responses_ );
    for ( id response_ in responses_ )
    {
        if ( [ response_ isGameStartedResponse ] && self.sessionState.startGameState == nil )
        {
            MWStartGameState* state_ = [ MWStartGameState startGameStateWithDictionary: response_ ];
            state_.youFirst = [ self.login isEqualToString: state_.currentPlayer ];
            self.sessionState.startGameState = state_;

            if ( self.startGameCallback )
            {
                self.startGameCallback( state_, nil );
                self.startGameCallback = nil;
            }
            //GTODO notify callback with Game started with state_
        }
        else if ( [ response_ isCurrentGameSateResponse ] )
        {
            NSArray* cids_ = self.sessionState.startGameState.users;
            MWCurrentGameState* currState_ = [ MWCurrentGameState currentGameStateWithDictionary: response_
                                                                                         userCid: self.login//GTODO change login on cid here
                                                                                       usersCids: cids_ ];
            self.currentGameState = currState_;

            if ( self.stateGameCallback )
            {
                self.stateGameCallback( currState_, nil );
                self.stateGameCallback = nil;
            }
        }
        else if ( [ response_ isGetSymbolsResponse ] )
        {
            NSArray* symbols_ = [ NSArray arraySymbolsWithDictionary: response_ ];

            if ( self.symbolsCallback )
            {
                self.symbolsCallback( symbols_, nil );
                self.symbolsCallback = nil;
            }
        }
    }
}

-(void)startBackwardRequest
{
    self.sessionState.active = YES;

    JFFAsyncOperation loader_ = [ self privateGetSrvState2 ];
    loader_ = asyncOperationWithFinishHookBlock( loader_
                                                , ^( id result_
                                                    , NSError* error_
                                                    , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        if ( [ result_ isKindOfClass: [ NSArray class ] ] )
        {
            [ self processServerResponse: result_ ];
        }
        done_callback_( result_, error_ );
    } );

    PredicateBlock predicate_ = ^BOOL( id object_ )
    {
        return self.sessionState.active;
    };

    //GTODO remove repeatAsyncOperation ( memory leak )
    repeatAsyncOperation( loader_
                         , predicate_
                         , 0.1
                         , -1 )( nil, nil, ^( id result, NSError* error )
    {
        NSLog( @"Server ping Stoped" );
    } );
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
                if ( result_ )
                {
                    context_.result = result_;
                    self.sessionState.sid = result_;
                    [ self startBackwardRequest ];
                }
                done_callback_( result_, error_ );
            } );

            return loader_( progress_callback_, cancel_callback_, done_callback_ );
        };

        loader_ = [ self.sessionState asyncOperationForPropertyWithName: @"sid"
                                                         asyncOperation: loader_ ];

      return loader_( progress_callback_, cancel_callback_, done_callback_ );
   } copy ] autorelease ];
}

-(JFFAsyncOperation)playBattleground
{
    JFFAsyncOperation auth_loader_ = [ self authLoader ];

    JFFAsyncOperation play_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                      , JFFCancelAsyncOperationHandler cancel_callback_
                                      , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        JFFAsyncOperation loader_ = [ self.api playBattlegroundForSid: self.sessionState.sid ];
        return loader_( progress_callback_
                       , cancel_callback_
                       , done_callback_ );
    };

    JFFAsyncOperation ping_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                       , JFFCancelAsyncOperationHandler cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        if ( self.sessionState.startGameState )
        {
            if ( done_callback_ )
                done_callback_( self.sessionState.startGameState, nil );
            return JFFEmptyCancelAsyncOperationBlock;
        }
        JFFAsyncOperation loader_ = [ self.api keepAliveForSid: self.sessionState.sid ];

        PredicateBlock predicate_ = ^BOOL( id object_ )
        {
            return self.sessionState.startGameState == nil;
        };

        loader_ = repeatAsyncOperation( loader_
                                       , predicate_
                                       , 1
                                       , 60 );

        loader_ = asyncOperationWithFinishHookBlock( loader_
                                                    , ^( id result_
                                                        , NSError* error_
                                                        , JFFDidFinishAsyncOperationHandler done_callback_ )
        {
            if ( result_ )
            {
                result_ = self.sessionState.startGameState;
            }
            done_callback_( result_, error_ );
        } );

        return loader_( progress_callback_
                       , cancel_callback_
                       , done_callback_ );
    };

    return sequenceOfAsyncOperations( auth_loader_
                                     , play_loader_
                                     , ping_loader_
                                     , nil );
}

-(JFFAsyncOperation)privateGetSymbolsSrvState
{
    return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                 , JFFCancelAsyncOperationHandler cancel_callback_
                 , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        done_callback_ = [ [ done_callback_ copy ] autorelease ];
        self.symbolsCallback = done_callback_;
        cancel_callback_ = [ [ cancel_callback_ copy ] autorelease ];
        return [ [ ^void( BOOL canceled_ )
        {
            if ( cancel_callback_ )
                cancel_callback_( canceled_ );

            if ( self.symbolsCallback == done_callback_ )
            {
                self.symbolsCallback = nil;
            }
        } copy ] autorelease ];
    } copy ] autorelease ];
}

-(JFFAsyncOperation)secondResultForFirstLoader:( JFFAsyncOperation )firstLoader_
                                  secondLoader:( JFFAsyncOperation )secondLoader_
{
    JFFResultContext* context_ = [ [ JFFResultContext new ] autorelease ];

    secondLoader_ = asyncOperationWithFinishHookBlock( secondLoader_
                                                      , ^( id result_
                                                          , NSError* error_
                                                          , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        context_.result = result_;
        context_.error  = error_;
        done_callback_( result_, error_ );
    } );

    JFFAsyncOperation loader_ = failOnFirstErrorGroupOfAsyncOperations( firstLoader_
                                                                       , secondLoader_
                                                                       , nil );

    JFFAsyncOperation resultLoader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                        , JFFCancelAsyncOperationHandler cancel_callback_
                                        , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        if ( done_callback_ )
            done_callback_( context_.result, nil );
        return JFFEmptyCancelAsyncOperationBlock;
    };
    return sequenceOfAsyncOperations( loader_, resultLoader_, nil );
}

-(JFFAsyncOperation)getSymbolsCount:( NSUInteger )count_
{
    JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                      , JFFCancelAsyncOperationHandler cancel_callback_
                                      , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        return [ self.api getSymbolsWithSid: self.sessionState.sid
                                      count: count_ ]( progress_callback_
                                                      , cancel_callback_
                                                      , done_callback_ );
    };

    return [ self secondResultForFirstLoader: cmd_loader_
                                secondLoader: [ self privateGetSymbolsSrvState ] ];
}

//GTODO implement
-(JFFAsyncOperation)privateExitGame
{
   return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                , JFFCancelAsyncOperationHandler cancel_callback_
                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      return [ self.api exitGameWithSid: self.sessionState.sid ]( progress_callback_
                                                                 , cancel_callback_
                                                                 , done_callback_ );
   } copy ] autorelease ];
}

-(JFFAsyncOperation)privateGetNextStepSrvState
{
    return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                 , JFFCancelAsyncOperationHandler cancel_callback_
                 , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        done_callback_ = [ [ done_callback_ copy ] autorelease ];
        self.stateGameCallback = done_callback_;
        cancel_callback_ = [ [ cancel_callback_ copy ] autorelease ];
        return [ [ ^void( BOOL canceled_ )
        {
            if ( cancel_callback_ )
                cancel_callback_( canceled_ );

            if ( self.stateGameCallback == done_callback_ )
            {
                self.stateGameCallback = nil;
            }
        } copy ] autorelease ];
    } copy ] autorelease ];
}

-(JFFAsyncOperation)doStepWithSymbsAndCoords:( NSArray* )step_
                                      scores:( NSUInteger )points_
{
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

    return [ self secondResultForFirstLoader: cmd_loader_
                                secondLoader: [ self privateGetNextStepSrvState ] ];
}

-(JFFAsyncOperation)skipStep
{
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

    return [ self secondResultForFirstLoader: cmd_loader_
                                secondLoader: [ self privateGetNextStepSrvState ] ];
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
