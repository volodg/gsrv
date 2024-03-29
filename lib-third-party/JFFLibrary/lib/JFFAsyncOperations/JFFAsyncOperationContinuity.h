#import <JFFAsyncOperations/JFFAsyncOperationsBlockDefinitions.h>
#import <JFFUtils/Blocks/JFFUtilsBlockDefinitions.h>

@class NSArray;

#ifdef __cplusplus
extern "C" {
#endif

///////////////////////////////////// SEQUENCE /////////////////////////////////////

//calls loaders while success
JFFAsyncOperation sequenceOfAsyncOperations( JFFAsyncOperation first_loader_
                                            , JFFAsyncOperation second_loader_, ... );

JFFAsyncOperation sequenceOfAsyncOperationsArray( NSArray* loaders_ );

/////////////////////////////////// TRY SEQUENCE ///////////////////////////////////

//calls loaders untill success
JFFAsyncOperation trySequenceOfAsyncOperations( JFFAsyncOperation first_loader_
                                               , JFFAsyncOperation second_loader_, ... );

JFFAsyncOperation trySequenceOfAsyncOperationsArray( NSArray* loaders_ );

/////////////////////////////////////// GROUP //////////////////////////////////////

//calls finish callback when all loaders finished
//result of group is undefined for success result
JFFAsyncOperation groupOfAsyncOperations( JFFAsyncOperation first_loader_
                                         , JFFAsyncOperation second_loader_, ... );

JFFAsyncOperation groupOfAsyncOperationsArray( NSArray* loaders_ );

///////////////////////////// FAIL ON FIRST ERROR GROUP ////////////////////////////

//calls finish callback when all loaders success finished or when any of them is failed
//result of group is undefined for success result
JFFAsyncOperation failOnFirstErrorGroupOfAsyncOperations( JFFAsyncOperation first_loader_
                                                         , JFFAsyncOperation second_loader_, ... );

JFFAsyncOperation failOnFirstErrorGroupOfAsyncOperationsArray( NSArray* loaders_ );

///////////////////////// ADD OBSERVERS OF ASYNC OP. RESULT ////////////////////////

//finish_callback_block_ called before loader_'s JFFDidFinishAsyncOperationHandler
JFFAsyncOperation asyncOperationWithFinishCallbackBlock( JFFAsyncOperation loader_
                                                        , JFFDidFinishAsyncOperationHandler finish_callback_block_ );

//finish_callback_hook_ called instead loader_'s JFFDidFinishAsyncOperationHandler
JFFAsyncOperation asyncOperationWithFinishHookBlock( JFFAsyncOperation loader_
                                                    , JFFDidFinishAsyncOperationHook finishCallbackHook_ );

//done_callback_hook_ called an cancel or finish loader_'s callbacks
JFFAsyncOperation asyncOperationWithDoneBlock( JFFAsyncOperation loader_
                                              , JFFSimpleBlock doneCallbackHook_ );

JFFAsyncOperation asyncOperationWithResult( id result_ );

///////////////////////// AUTO REPEAT CIRCLE ////////////////////////

JFFAsyncOperation repeatAsyncOperation( JFFAsyncOperation loader_
                                       , PredicateBlock predicate_
                                       , NSTimeInterval delay_
                                       , NSInteger max_repeat_count_ );

JFFAsyncOperation asyncOperationAfterDelay( NSTimeInterval delay_
                                           , JFFAsyncOperation loader_ );

#ifdef __cplusplus
}
#endif
