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

@implementation JEitherMonad

@synthesize error = _error;
@synthesize value = _value;

-(id)initWithError:( NSError* )error_
             value:( id )value_
{
   self = [ super init ];

   if ( self )
   {
      _error = [ error_ retain ];
      _value = [ value_ retain ];
   }

   return self;
}

+(id)eitherMonadWithError:( NSError* )error_
                    value:( id )value_
{
   return [ [ [ self alloc ] initWithError: error_
                                     value: value_ ] autorelease ];
}

-(id< JMonad >)bindOperation:( MonadOperation )op_
{
   return _error ? self : op_( _value );
}

-(id< JMonad >)bindVoidOperation:( MonadVoidOperation )op_;
{
   return _error ? self : op_();
}

+(id< JMonad >)monadForValue:( id )value_a_
{
   return [ self eitherMonadWithError: nil
                                value: value_a_ ];
}

@end
