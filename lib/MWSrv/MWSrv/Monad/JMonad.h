#import <Foundation/Foundation.h>

@protocol JMonad < NSObject >

@required

//(>>=)  :: m a -> (a -> m b) -> m b 
typedef id< JMonad > (^MonadOperation)( id value_a_ );
-(id< JMonad >)bindOperation:( MonadOperation )op_;

//(>>)   :: m a -> m b -> m b
typedef id< JMonad > (^MonadVoidOperation)( void );
-(id< JMonad >)bindVoidOperation:( MonadVoidOperation )op_;

//return :: a -> m a 
+(id< JMonad >)monadForValue:( id )value_a_;

@end

@interface JMaybeMonad : NSObject < JMonad >

@property ( nonatomic, retain, readonly ) id value;

+(id)maybeMonadWithValue:( id )value_;

@end
