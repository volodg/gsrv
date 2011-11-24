#import "JMonad.h"

@implementation JMaybeMonad

@synthesize value = _value;

-(id)initWithValue:( id )value_
{
   self = [ super init ];

   if ( self )
   {
      _value = [ value_ retain ];
   }

   return self;
}

+(id)maybeMonadWithValue:( id )value_
{
   return [ [ [ self alloc ] initWithValue: value_ ] autorelease ];
}

-(id< JMonad >)bindOperation:( MonadOperation )op_
{
   return _value ? op_( _value ) : self;
}

-(id< JMonad >)bindVoidOperation:( MonadVoidOperation )op_;
{
   return _value ? op_() : self;
}

+(id< JMonad >)monadForValue:( id )value_a_
{
   return [ self maybeMonadWithValue: value_a_ ];
}

@end
