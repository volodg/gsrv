#import "MWSession.h"

#import "MWApi.h"
#import "MWStartGameState.h"
#import "MWCurrentGameState.h"
#import "MWSessionGameState.h"

#import "NSArray+Parser.h"
#import "NSObject+Parser.h"
#import "MWGameStatistic+Parser.h"
#import "MWDoStepState+doStepStateWithPrevState.h"
#import "MWFinishGameStatistic+Parser.h"

typedef id (^MSResponseAnalizer)( id data_, NSError** error_ );

@interface MWPredicateAndCallback : NSObject

@property ( nonatomic, copy ) JFFDidFinishAsyncOperationHandler callback;
@property ( nonatomic, copy ) PredicateBlock predicate;

@end

@implementation MWPredicateAndCallback

@synthesize callback  = _callback;
@synthesize predicate = _predicate;

-(void)dealloc
{
    [ _callback  release ];
    [ _predicate release ];

    [ super dealloc ];
}

@end

@interface MWSession ()

@property ( nonatomic, assign ) MWGameType gameType;
@property ( nonatomic, retain ) NSDate* lastLoginDate;//GTODO 5 minutes after last request
@property ( nonatomic, retain ) MWApi* api;
@property ( nonatomic, retain ) NSString* login;
@property ( nonatomic, retain ) MWSessionGameState* sessionState;
@property ( nonatomic, copy ) JFFDidFinishAsyncOperationHandler statisticCallback;//GTODO remove

@property ( nonatomic, retain ) NSMutableArray* callbacks;
@property ( nonatomic, retain ) NSArray* finishGameStatistic;
@property ( nonatomic, retain ) NSString* sid;

-(JFFAsyncOperation)privateExitGame;
-(JFFAsyncOperation)keepAliveCmd;
-(JFFAsyncOperation)loaderForResponsePredicate:( PredicateBlock )predicate_
                                      analyzer:( MSResponseAnalizer )analyzer_;

@end

@implementation MWSession

@synthesize sid                 = _sid;
@synthesize gameType            = _gameType;
@synthesize lastLoginDate       = _lastLoginDate;
@synthesize api                 = _api;
@synthesize login               = _login;
@synthesize sessionState        = _sessionState;//GTODO rename on game state
@synthesize statisticCallback   = _statisticCallback;
@synthesize callbacks           = _callbacks;
@synthesize finishGameCallback  = _finishGameCallback;
@synthesize finishGameStatistic = _finishGameStatistic;

//GTODO add connection latency
-(void)dealloc
{
    [ _sid                 release ];
    [ _lastLoginDate       release ];
    [ _api                 release ];
    [ _login               release ];
    [ _sessionState        release ];
    [ _statisticCallback   release ];
    [ _callbacks           release ];
    [ _finishGameCallback  release ];
    [ _finishGameStatistic release ];

    [ super dealloc ];
}

-(id)initWithLogin:( NSString* )login_
{
    self = [ super init ];

    if ( self )
    {
        self.callbacks = [ NSMutableArray array ];
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
   //NSTimeInterval interval_ = [ self.lastLoginDate timeIntervalSinceNow ];
   return NO;//interval_ < -4.8 * 60;
}

-(JFFAsyncOperation)privateGetSrvState
{
    __block MWSession* self_ = self;
    return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                 , JFFCancelAsyncOperationHandler cancel_callback_
                 , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        NSAssert( self_.sid, @"sid can not be empty" );
        JFFAsyncOperation loader_ = [ self_.api getSrvStateWithSid: self_.sid ];
        done_callback_ = [ [ done_callback_ copy ] autorelease ];
        return loader_( progress_callback_, cancel_callback_, ^void( id result_, NSError* error_ )
        {
            if ( done_callback_ )
                done_callback_( result_, error_ );
        } );
    } copy ] autorelease ];
}

-(BOOL)processCallbacksWithResponse:( id )response_
{
    for ( NSUInteger index_ = 0; index_ < [ self.callbacks count ]; ++index_ )
    {
        MWPredicateAndCallback* data_ = [ self.callbacks objectAtIndex: index_ ];
        if ( data_.predicate( response_ ) )
        {
            JFFDidFinishAsyncOperationHandler handler_ = [ data_.callback copy ];
            [ self.callbacks removeObjectAtIndex: index_ ];
            handler_( response_, nil );
            [ handler_ release ];
            return YES;
        }
    }
    return NO;
}

-(void)processServerResponse:( NSArray* )responses_
{
    NSLog( @"processServerResponse user: %@ data: %@", self.login, responses_ );
    for ( id response_ in responses_ )
    {
        if ( [ response_ isStatisticResponse ] )
        {
            MWGameStatistic* result_ = [ MWGameStatistic gameStatisticWithDict: response_ ];

            if ( self.statisticCallback )
            {
                JFFDidFinishAsyncOperationHandler handler_ = [ [ self.statisticCallback copy ] autorelease ];
                if ( handler_ == self.statisticCallback )
                    self.statisticCallback = nil;
                handler_( result_, nil );
            }
        }
        else if ( [ self processCallbacksWithResponse: response_ ] )
        {
            continue;
        }
    }
}

-(void)startBackwardRequest
{
    __block MWSession* self_ = self;

    JFFAsyncOperation loader_ = [ self privateGetSrvState ];
    loader_ = asyncOperationWithFinishHookBlock( loader_
                                                , ^( id result_
                                                    , NSError* error_
                                                    , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        if ( [ result_ isKindOfClass: [ NSArray class ] ] )
        {
            [ self_ processServerResponse: result_ ];
        }
        done_callback_( result_, error_ );
    } );

    PredicateBlock predicate_ = ^BOOL( id object_ )
    {
        return YES;
    };

    loader_ = repeatAsyncOperation( loader_
                                   , predicate_
                                   , 0.1
                                   , -1 );

    loader_( nil, nil, ^( id result, NSError* error )
    {
        NSLog( @"Server ping Stoped" );
    } );
}

-(void)gameFinishedWithStatistic:( NSArray* )statistic_
{
    self.finishGameStatistic = [ statistic_ sortedArrayUsingComparator: ^NSComparisonResult( id obj1, id obj2 )
    {
        NSNumber* points1_ = [ NSNumber numberWithInt: [ obj1 points ] ];
        NSNumber* points2_ = [ NSNumber numberWithInt: [ obj2 points ] ];
        return [ points2_ compare: points1_ ];
    } ];

    self.sessionState = nil;
}

-(void)setFinishGameCallback
{
    NSAssert( _sessionState.startGameState, @"should ne inited" );

    MWPredicateAndCallback* predicateAndCallback_ = [ [ MWPredicateAndCallback new ] autorelease ];

    predicateAndCallback_.callback  = ^( id result_, NSError* error_ )
    {
        MWFinishGameStatistic* finishGame_ = [ MWFinishGameStatistic finishGameStatisticWithDict: result_ ];
        [ self.sessionState.statisticByCid setObject: finishGame_ forKey: finishGame_.playerNick ];

        if ( [ self.sessionState.statisticByCid count ] == [ self.userNames count ] )
        {
            [ self gameFinishedWithStatistic: [ self.sessionState.statisticByCid allValues ] ];
            if ( self.finishGameCallback )
            {
                JFFDidFinishAsyncOperationHandler finishGameCallback_ = [ [ self.finishGameCallback copy ] autorelease ];
                self.finishGameCallback = nil;
                finishGameCallback_( self.finishGameStatistic, nil );
            }
        }
    };
    predicateAndCallback_.predicate = ^BOOL( id result_ )
    {
        return [ result_ isGameFinishedResponse ];
    };

    for ( NSUInteger index_ = 0; index_ < [ self.userNames count ]; ++index_ )
    {
        [ self.callbacks addObject: predicateAndCallback_ ];
    }
}

-(void)initializeAfterAuthWithSid:( NSString* )sid_
{
    self.sid = sid_;
    [ self startBackwardRequest ];
}

//GTODO refactor
-(JFFAsyncOperation)authLoader
{
    __block MWSession* self_ = self;
    JFFAsyncOperation loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                  , JFFCancelAsyncOperationHandler cancel_callback_
                                  , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        JFFAsyncOperation loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                      , JFFCancelAsyncOperationHandler cancel_callback_
                                      , JFFDidFinishAsyncOperationHandler done_callback_ )
        {
            JFFAsyncOperation loader_ = [ self_.api authWithLogin: self_.login sid: nil ];
            loader_ = asyncOperationWithFinishHookBlock( loader_
                                                        , ^( id result_
                                                            , NSError* error_
                                                            , JFFDidFinishAsyncOperationHandler done_callback_ )
            {
                if ( result_ )
                {
                    [ self_ initializeAfterAuthWithSid: result_ ];
                }
                done_callback_( result_, error_ );
            } );

            return loader_( progress_callback_, cancel_callback_, done_callback_ );
        };

        loader_ = [ self_ asyncOperationForPropertyWithName: @"sid"
                                             asyncOperation: loader_ ];

        return loader_( progress_callback_, cancel_callback_, done_callback_ );
    };

    self.sessionState = [ [ MWSessionGameState new ] autorelease ];
    return [ self.sessionState autoCancelOnDeallocAsyncOperation: loader_ ];
}

-(JFFAsyncOperation)loaderForResponsePredicate:( PredicateBlock )predicate_
                                      analyzer:( MSResponseAnalizer )analyzer_
{
    __block MWSession* self_ = self;

    analyzer_  = [ [ analyzer_  copy ] autorelease ];
    predicate_ = [ [ predicate_ copy ] autorelease ];

    return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                 , JFFCancelAsyncOperationHandler cancelCallback_
                 , JFFDidFinishAsyncOperationHandler doneCallback_ )
    {
        MSResponseAnalizer lcAnalyzer_ = [ [ ^id( id data_, NSError** error_ )
        {
            if ( analyzer_ && data_ )
                return analyzer_( data_, error_ );
            return data_;
        } copy ] autorelease ];

        doneCallback_ = [ [ doneCallback_ copy ] autorelease ];
        doneCallback_ = ^( id result_, NSError* error_ )
        {
            if ( doneCallback_ )
            {
                NSError* localError_ = error_;
                result_ = lcAnalyzer_( result_, &localError_ );
                doneCallback_( result_, localError_ );
            }
        };

        MWPredicateAndCallback* predicateAndCallback_ = [ [ MWPredicateAndCallback new ] autorelease ];
        predicateAndCallback_.callback  = doneCallback_;
        predicateAndCallback_.predicate = predicate_;
        [ self_.callbacks addObject: predicateAndCallback_ ];

        cancelCallback_ = [ [ cancelCallback_ copy ] autorelease ];
        return [ [ ^void( BOOL canceled_ )
        {
            if ( cancelCallback_ )
                cancelCallback_( canceled_ );

            [ self_.callbacks removeObject: predicateAndCallback_ ];
        } copy ] autorelease ];
    } copy ] autorelease ];
}

-(JFFAsyncOperation)waitStartGameState
{
    PredicateBlock predicate_ = ^BOOL( id object_ )
    {
        if ( [ object_ isKeepAlive ] )
        {
            //GTODO move keepAliveCmd to sequence
            [ self keepAliveCmd ]( nil, nil, nil );
            return NO;
        }
        return [ object_ isGameStartedResponse ];
    };
    MSResponseAnalizer analyzer_ = ^id( id response_, NSError** error_ )
    {
        MWStartGameState* state_ = [ MWStartGameState startGameStateWithDictionary: response_ ];
        state_.youFirst = [ self.login isEqualToString: state_.currentPlayer ];
        self.sessionState.startGameState = state_;
        [ self setFinishGameCallback ];

        return state_;
    };
    return [ self loaderForResponsePredicate: predicate_
                                    analyzer: analyzer_ ];
}

-(JFFAsyncOperation)keepAliveCmd
{
    __block MWSession* self_ = self;
    return  [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                  , JFFCancelAsyncOperationHandler cancel_callback_
                  , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        JFFAsyncOperation loader_ = [ self_.api keepAliveForSid: self_.sid ];
        return loader_( progress_callback_
                       , cancel_callback_
                       , done_callback_ );
    } copy ] autorelease ];
}

-(JFFAsyncOperation)playBattleground
{
    __block MWSession* self_ = self;

    JFFAsyncOperation auth_loader_ = [ self authLoader ];

    JFFAsyncOperation play_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                      , JFFCancelAsyncOperationHandler cancel_callback_
                                      , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        self_.gameType = MWGameTypeBattleground;
        JFFAsyncOperation loader_ = [ self_.api playBattlegroundForSid: self_.sid ];
        return loader_( progress_callback_
                       , cancel_callback_
                       , done_callback_ );
    };

    JFFAsyncOperation loader_ = sequenceOfAsyncOperations( auth_loader_
                                                          , play_loader_
                                                          , [ self waitStartGameState ]
                                                          , nil );

    return [ self.sessionState autoCancelOnDeallocAsyncOperation: loader_ ];
}

-(JFFAsyncOperation)playDuel
{
    __block MWSession* self_ = self;

    JFFAsyncOperation auth_loader_ = [ self authLoader ];

    JFFAsyncOperation play_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                       , JFFCancelAsyncOperationHandler cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        self_.gameType = MWGameTypeDuel;
        JFFAsyncOperation loader_ = [ self_.api playDuelForSid: self_.sid ];
        return loader_( progress_callback_
                       , cancel_callback_
                       , done_callback_ );
    };

    JFFAsyncOperation loader_ = sequenceOfAsyncOperations( auth_loader_
                                                          , play_loader_
                                                          , [ self waitStartGameState ]
                                                          , nil );

    return [ self.sessionState autoCancelOnDeallocAsyncOperation: loader_ ];
}

-(JFFAsyncOperation)privateGetSymbolsSrvState
{
    PredicateBlock predicate_ = ^BOOL( id object_ )
    {
        return [ object_ isGetSymbolsResponse ];
    };
    MSResponseAnalizer analyzer_ = ^id( id response_, NSError** error_ )
    {
        return [ NSArray arraySymbolsWithDictionary: response_ ];
    };
    return [ self loaderForResponsePredicate: predicate_
                                    analyzer: analyzer_ ];

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
                      returnSymbols:( NSArray* )symbols_
{
    JFFAsyncOperation cmd_loader1_ = [ self.api returnSymbolsWithSid: self.sid
                                                             symbols: symbols_ ];
    JFFAsyncOperation cmd_loader2_ = nil;
    if ( count_ == 0 )
    {
        cmd_loader2_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                         , JFFCancelAsyncOperationHandler cancel_callback_
                         , JFFDidFinishAsyncOperationHandler done_callback_ )
        {
            if ( done_callback_ )
                done_callback_( [ NSArray array ], nil );
            return JFFEmptyCancelAsyncOperationBlock;
        };
    }
    else
    {
        cmd_loader2_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                         , JFFCancelAsyncOperationHandler cancel_callback_
                         , JFFDidFinishAsyncOperationHandler done_callback_ )
        {
            return [ self.api getSymbolsWithSid: self.sid
                                          count: count_ ]( progress_callback_
                                                          , cancel_callback_
                                                          , done_callback_ );
        };
        cmd_loader2_ = [ self secondResultForFirstLoader: cmd_loader2_
                                            secondLoader: [ self privateGetSymbolsSrvState ] ];
    }

    JFFAsyncOperation loader_ = sequenceOfAsyncOperations( cmd_loader1_, cmd_loader2_, nil );
    return [ self.sessionState autoCancelOnDeallocAsyncOperation: loader_ ];
}

//GTODO implement
-(JFFAsyncOperation)privateExitGame
{
   JFFAsyncOperation loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                 , JFFCancelAsyncOperationHandler cancel_callback_
                                 , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      return [ self.api exitGameWithSid: self.sid ]( progress_callback_
                                                    , cancel_callback_
                                                    , done_callback_ );
   };
   return [ self.sessionState autoCancelOnDeallocAsyncOperation: loader_ ];
}

-(JFFAsyncOperation)privateGetNextStepSrvState
{
    PredicateBlock predicate_ = ^BOOL( id object_ )
    {
        return [ object_ isCurrentGameSateResponse ];
    };
    MSResponseAnalizer analyzer_ = ^id( id response_, NSError** error_ )
    {
        NSUInteger previousLength = [ self.sessionState.currentGameState.symbolsAndCoods count ];
        MWCurrentGameState* currState_ = [ MWCurrentGameState currentGameStateWithDictionary: response_ ];

        if ( [ currState_.symbolsAndCoods count ] < previousLength )
        {
            if ( error_ )
            {
                *error_ = [ JFFError errorWithDescription: @"Invalid server response" ];
            }
            return nil;
        }

        NSRange range = NSMakeRange( previousLength, [ currState_.symbolsAndCoods count ] - previousLength );
        currState_.symbolsAndCoods = [ currState_.symbolsAndCoods subarrayWithRange: range ];
        self.sessionState.currentGameState = currState_;

        return currState_;
    };
    JFFAsyncOperation loader_ = [ self loaderForResponsePredicate: predicate_
                                                         analyzer: analyzer_ ];
    return [ self.sessionState autoCancelOnDeallocAsyncOperation: loader_ ];
}

-(JFFAsyncOperation)doStepWithSymbsAndCoords:( NSArray* )step_
                                       state:( MWDoStepState* )state_
{
    if ( [ step_ count ] == 0 )
        return [ self skipStep ];

    JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                      , JFFCancelAsyncOperationHandler cancel_callback_
                                      , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        MWDoStepState* passState_ = [ state_ doStepStateWithPrevState: self.sessionState.lastStepState ];
        self.sessionState.lastStepState = state_;
        return [ self.api doStepWithSid: self.sid
                         symbsAndCoords: step_
                                  state: passState_ ]( progress_callback_
                                                      , cancel_callback_
                                                      , done_callback_ );
    };

    cmd_loader_ = [ self secondResultForFirstLoader: cmd_loader_
                                       secondLoader: [ self privateGetNextStepSrvState ] ];
    return [ self.sessionState autoCancelOnDeallocAsyncOperation: cmd_loader_ ];
}

-(JFFAsyncOperation)skipStep
{
    JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                      , JFFCancelAsyncOperationHandler cancel_callback_
                                      , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        JFFAsyncOperation loader_ = [ self.api doStepWithSid: self.sid
                                              symbsAndCoords: nil
                                                       state: self.sessionState.lastStepState ];
        return loader_( progress_callback_
                       , cancel_callback_
                       , done_callback_ );
    };

    cmd_loader_ = [ self secondResultForFirstLoader: cmd_loader_
                                       secondLoader: [ self privateGetNextStepSrvState ] ];
    return [ self.sessionState autoCancelOnDeallocAsyncOperation: cmd_loader_ ];
}

-(JFFAsyncOperation)privateGetStatisticForCid:( NSString* )cid_
{
    return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                 , JFFCancelAsyncOperationHandler cancel_callback_
                 , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        done_callback_ = [ [ done_callback_ copy ] autorelease ];
        done_callback_ = ^( id result_, NSError* error_ )
        {
            MWGameStatistic* statistic_ = result_;
            statistic_.cid = cid_;
            [ self.sessionState.statisticByCid setObject: statistic_ forKey: statistic_.cid ];
            if ( done_callback_ )
                done_callback_( result_, error_ );
        };
        done_callback_ = [ [ done_callback_ copy ] autorelease ];
        self.statisticCallback = done_callback_;
        cancel_callback_ = [ [ cancel_callback_ copy ] autorelease ];
        return [ [ ^void( BOOL canceled_ )
        {
            if ( cancel_callback_ )
                cancel_callback_( canceled_ );

            if ( self.statisticCallback == done_callback_ )
            {
                self.statisticCallback = nil;
            }
        } copy ] autorelease ];
    } copy ] autorelease ];
}

/*-(JFFAsyncOperation)statistic
{
    JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                      , JFFCancelAsyncOperationHandler cancel_callback_
                                      , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        JFFAsyncOperation loader_ = [ self.api statisticForSid: self.sessionState.sid ];
        return loader_( progress_callback_
                       , cancel_callback_
                       , done_callback_ );
    };

    cmd_loader_ = [ self secondResultForFirstLoader: cmd_loader_
                                       secondLoader: [ self privateGetStatisticForCid: cid_ ] ];
    return [ self.sessionState autoCancelOnDeallocAsyncOperation: cmd_loader_ ];
}

-(JFFAsyncOperation)statisticLoader
{
    NSArray* statisticLoaders_ = [ self.sessionState.startGameState.users map: ^id( id cid_ )
    {
        return [ self statistic ];
    } ];
    JFFAsyncOperation loader_ = sequenceOfAsyncOperationsArray( statisticLoaders_ );

    JFFDidFinishAsyncOperationHook finishCallbackHook_ = ^( id result_
                                                           , NSError* error_
                                                           , JFFDidFinishAsyncOperationHandler doneCallback_ )
    {
        if ( doneCallback_ )
        {
            result_ = result_ ? self.sessionState.statisticByCid : nil;
            doneCallback_( self.sessionState.statisticByCid, error_ );
        }
    };

    return asyncOperationWithFinishHookBlock( loader_, finishCallbackHook_ );
}*/
//self.sessionState.startGameState

-(JFFAsyncOperation)waitStep
{
    return [ self privateGetNextStepSrvState ];
}

-(void)exitGame
{
   if ( self.sid )
      [ self.api exitGameWithSid: self.sid ]( nil, nil, nil );
   self.sessionState = nil;
}

////////////////////////////////////////////////////////////////////////////////

-(NSArray*)allPoints
{
    return self.sessionState.currentGameState.allPoints;
}

-(NSArray*)userNames
{
    return self.sessionState.startGameState.users;
}

-(NSArray*)startPoints
{
    return self.sessionState.startGameState.startPoints;
}

-(JFFAsyncOperation)mergeLoader1:( JFFAsyncOperation )loader1_
                         loader2:( JFFAsyncOperation )loader2_
                         loader3:( JFFAsyncOperation )loader3_
{
    return sequenceOfAsyncOperations( loader1_, loader2_, loader3_, nil );
}

@end
