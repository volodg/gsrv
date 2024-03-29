#import "MWApi.h"

#import "NSString+RandomString.h"

#import "JMonad.h"

#import "MWSymb.h"
#import "MWSymbWithCoords.h"
#import "MWDoStepState.h"

#import <AMFUnarchiver.h>

static NSUInteger sidLength_ = 32;
//static NSString* const host_format_ = @"http://188.95.152.130:3333/%@";
//static NSString* const host_format_ = @"http://test.bwf.org.ua:3333/%@";
static NSString* const host_format_ = @"http://188.95.152.130:3333/%@";

@interface MWSymbWithCoords (MWApi)
@end

@implementation MWSymbWithCoords (MWApi)

-(NSString*)toJsonCommandWithIndex:( NSUInteger )index_
{
   return [ NSString stringWithFormat: @"\"x%d\":%d, \"y%d\":%d, \"state%d\":\"%d\", \"sym%d\":\"%@\""
           , index_, self.x
           , index_, self.y
           , index_, self.symb.state
           , index_, self.symb.symb ];
}

@end

@interface MWSymb (MWApi)
@end

@implementation MWSymb (MWApi)

-(NSString*)toJsonCommandWithIndex:( NSUInteger )index_
{
    return [ NSString stringWithFormat: @"\"sym%d\":\"%@\", \"state%d\":\"%d\""
            , index_, self.symb
            , index_, self.state ];
}

@end

@interface NSArray (MWApi)
@end

@implementation NSArray (MWApi)

-(NSString*)toJsonCommand
{
   __block NSUInteger index_ = 0;
   NSArray* result_ = [ self map: ^id( id object_ )
   {
      return [ object_ toJsonCommandWithIndex: ++index_ ];
   } ];
   return [ result_ componentsJoinedByString: @", " ];
}

@end

@interface MWDoStepState (MWApi)
@end

@implementation MWDoStepState (MWApi)

-(NSString*)toJsonCommand
{
    return [ NSString stringWithFormat: @"\"point\": \"%d\", \"stused\": \"%d\", \"mnused\": \"%d\", \"tperturn\": \"%f\", \"jadvise\": \"%d\", \"sp1\": \"%d\", \"sp2\": \"%d\", \"sp3\": \"%d\", \"sp4\": \"%d\", \"elem1\": \"%d\", \"elem2\": \"%d\", \"elem3\": \"%d\", \"elem4\": \"%d\", \"bnst1\": \"%d\", \"bnst2\": \"%d\", \"bnst3\": \"%d\", \"bnst4\": \"%d\""
            , self.points
            , self.stused
            , self.mnused
            , self.tperturn
            , self.jadvise
            , self.sp1
            , self.sp2
            , self.sp3
            , self.sp4
            , self.elem1
            , self.elem2
            , self.elem3
            , self.elem4
            , self.bnst1
            , self.bnst2
            , self.bnst3
            , self.bnst4
            ];
}

@end

@interface JEitherMonad (MWApi)
@end

@implementation JEitherMonad (MWApi)

-(void)notifyDoneBlock:( JFFDidFinishAsyncOperationHandler )block_
{
   if ( block_ )
   {
      block_( self.value, self.error );
   }
}

@end

@interface NSURL (MWApi)

+(id)URLWithSid:( NSString* )sid_;

@end

@implementation NSURL (MWApi)

+(id)URLWithSid:( NSString* )sid_
{
   NSString* str_ = [ NSString stringWithFormat: host_format_, sid_ ];
   return [ NSURL URLWithString: str_ ];
}

@end

@interface MWApi ()

@property ( nonatomic, retain, readonly ) NSDictionary* headers;

@end

@implementation MWApi

@synthesize headers = _headers;

-(void)dealloc
{
   [ _headers release ];

   [ super dealloc ];
}

-(id)init
{
   self = [ super init ];

   if ( self )
   {
      srand ( time(NULL) );
   }

   return self;
}

-(NSDictionary*)headers
{
   if ( !_headers )
   {
      _headers = [ [ NSDictionary alloc ] initWithObjectsAndKeys:
                  @"application/json", @"Content-type"
                  , nil ];
   }
   return _headers;
}

-(id< JMonad >)monadForResponse:( id< JNUrlResponse > )response_
                          error:( NSError* )error_
{
   id value_ = error_ ? nil : response_;
   if ( error_ == nil && 200 != response_.statusCode )
   {
      value_ = nil;
      NSString* error_format_ = @"Invalid auth response code: %d";
      NSString* error_description_ = [ NSString stringWithFormat:
                                      error_format_
                                      , response_.statusCode ];
      error_ = [ JFFError errorWithDescription: error_description_ ];
   }
   return [ JEitherMonad eitherMonadWithError: error_
                                        value: value_ ];
}

-(JFFAsyncOperation)authWithLogin:( NSString* )login_
                              sid:( NSString* )sid_
{
    sid_ = sid_ ? sid_ : [ NSString randomStringWithLength: sidLength_ ];
    NSURL*    url_ = [ NSURL URLWithSid: sid_ ];

    NSString* post_format_ = @"{\"cmd\":\"enter\",\"env\":1,\"cid\":\"%@\",\"name\":\"%@\"}";
    NSString* post_        = [ NSString stringWithFormat: post_format_
                              , login_ ? login_ : @""
                              , login_ ? login_ : @"" ];
    NSData*   post_data_   = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

    NSLog( @"authWithLogin: %@", login_ );

    JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                         , post_data_
                                                         , self.headers );

    JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^void( id result_
                                                                 , NSError* error_
                                                                 , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        NSLog( @"done auth" );
        JEitherMonad* monad_ = [ [ self monadForResponse: result_ error: error_ ]
                                bindVoidOperation: ^id<JMonad>( void )
        {
            return [ JEitherMonad eitherMonadWithError: nil
                                                 value: sid_ ];
        } ];

        [ monad_ notifyDoneBlock: done_callback_ ];
    };

    return asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
}

-(JFFAsyncOperation)playBattlegroundForSid:( NSString* )sid_
{
    NSURL* url_ = [ NSURL URLWithSid: sid_ ];

    NSString* post_      = @"{\"cmd\":\"playBattleground\"}";
    NSData*   post_data_ = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

    NSLog( @"playBattleground: %@", post_ );

    JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                         , post_data_
                                                         , self.headers );

    JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^void( id result_
                                                                 , NSError* error_
                                                                 , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        NSLog( @"playBattleground done" );
        JEitherMonad* monad_ = [ self monadForResponse: result_ error: error_ ];
        [ monad_ notifyDoneBlock: done_callback_ ];
    };

    return asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
}

-(JFFAsyncOperation)playDuelForSid:( NSString* )sid_
{
    NSURL* url_ = [ NSURL URLWithSid: sid_ ];
    
    NSString* post_      = @"{\"cmd\":\"playDuel\"}";
    NSData*   post_data_ = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

    NSLog( @"playDuel: %@", post_ );

    JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                         , post_data_
                                                         , self.headers );

    JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^void( id result_
                                                                 , NSError* error_
                                                                 , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        JEitherMonad* monad_ = [ self monadForResponse: result_ error: error_ ];
        [ monad_ notifyDoneBlock: done_callback_ ];
    };

    return asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
}

-(JFFAsyncOperation)keepAliveForSid:( NSString* )sid_
{
    NSURL* url_ = [ NSURL URLWithSid: sid_ ];

    NSString* post_      = @"{\"cmd\":\"keepAlive\"}";
    NSData*   post_data_ = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

    NSLog( @"keepAlive: %@", post_ );

    JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                         , post_data_
                                                         , self.headers );

    JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^void( id result_
                                                                 , NSError* error_
                                                                 , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        JEitherMonad* monad_ = [ self monadForResponse: result_ error: error_ ];
        [ monad_ notifyDoneBlock: done_callback_ ];
    };

    return asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
}

-(NSArray*)splitResponseData:( NSData* )data_
{
   NSMutableArray* result_ = [ NSMutableArray new ];
   for ( NSUInteger index_ = 0; index_ < data_.length; )
   {
      NSData* size_data_ = [ data_ subdataWithRange: NSMakeRange( index_, sizeof( int ) ) ];
      int size_ = CFSwapInt32BigToHost(*(int*)([size_data_ bytes]));

      {
         NSRange chunkRange_ = NSMakeRange( index_ + sizeof( int ), size_ );
         NSData* chunk_ = [ data_ subdataWithRange: chunkRange_ ];
         [ result_ addObject: chunk_ ];
      }

      index_ += size_ + sizeof( int );
   }
   return [ result_ autorelease ];
}

-(JFFAsyncOperation)getSrvStateWithSid:( NSString* )sid_
{
   return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                , JFFCancelAsyncOperationHandler cancel_callback_
                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      //NSURL* url_ = [ NSURL URLWithString: @"http://test.bwf.org.ua:3333" ];
      NSURL* url_ = [ NSURL URLWithSid: sid_ ];

      static char data_[1] = { 0 };
      NSData* post_data_ = [ NSData dataWithBytes: data_ length: 1 ];

      NSMutableData* response_data_ = [ NSMutableData data ];

      JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                           , post_data_
                                                           , nil );

       JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^void( id result_
                                                                   , NSError* error_
                                                                   , JFFDidFinishAsyncOperationHandler done_callback_ )
      {
         JEitherMonad* monad_ = [ [ self monadForResponse: result_ error: error_ ]
         bindOperation: ^id<JMonad>( id value_a_ )
         {
            id chunks_ = [ self splitResponseData: response_data_ ];
            id value_ = [ chunks_ map: ^id( id chunk_ )
            {
               return [ AMFUnarchiver unarchiveObjectWithData: chunk_
                                                     encoding: kAMF3Encoding ];
            } ];
            return [ JEitherMonad eitherMonadWithError: nil
                                                 value: value_ ];
         } ];

         //NSLog( @"<<<1>>>getSrvStateWithSid resp: %@", monad_.value );
         //NSLog( @"<<<2>>>getSrvStateWithSid resp: %@ error: %@", result_, error_ );
         [ monad_ notifyDoneBlock: done_callback_ ];
      };

      progress_callback_ = [ [ progress_callback_ copy ] autorelease ];
      progress_callback_ = ^( id progress_data_ )
      {
          NSLog( @"got data: %@", [ [ NSString alloc ] initWithData: progress_data_
                                                           encoding: NSUTF8StringEncoding ] );
         [ response_data_ appendData: progress_data_ ];
         //NSString* str_ = [ [ [ NSString alloc ] initWithData: progress_data_ encoding: NSUTF8StringEncoding ] autorelease ];
         //NSLog( @"<<<1.1>>>getSrvStateWithSid chunk: %@", str_ );
         if ( progress_callback_ )
            progress_callback_( progress_data_ );
      };

      loader_ = asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
      return loader_( progress_callback_
                     , cancel_callback_
                     , done_callback_  );
   } copy ] autorelease ];
}

-(JFFAsyncOperation)validatedResponseLoaderWithUrl:( NSURL* )url_
                                          postData:( NSData* )data_
{
    JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                         , data_
                                                         , self.headers );

    JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^void( id result_
                                                                 , NSError* error_
                                                                 , JFFDidFinishAsyncOperationHandler done_callback_ )
    {
        JEitherMonad* monad_ = [ self monadForResponse: result_ error: error_ ];
        [ monad_ notifyDoneBlock: done_callback_ ];
    };

    return asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
}

-(JFFAsyncOperation)exitGameWithSid:( NSString* )sid_
{
    NSURL* url_ = [ NSURL URLWithSid: sid_ ];

    NSString* post_      = @"{\"cmd\":\"exitGame\"}";
    NSData*   post_data_ = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

    NSLog( @"exitGameWithSid post: %@", post_ );

    return [ self validatedResponseLoaderWithUrl: url_
                                        postData: post_data_ ];
}

-(JFFAsyncOperation)returnSymbolsWithSid:( NSString* )sid_
                                 symbols:( NSArray* )symbols_
{
    NSURL* url_ = [ NSURL URLWithSid: sid_ ];

    if ( [ symbols_ count ] == 0 )
        return asyncOperationWithResult( [ NSNull null ] );

    NSString* post_format_ = @"{\"cmd\":\"returnSymbols\", %@}";
    NSString* post_        = [ NSString stringWithFormat: post_format_, [ symbols_ toJsonCommand ] ];
    NSData*   post_data_   = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

    NSLog( @"returnSymbols post: %@ url: %@", post_, url_ );

    return [ self validatedResponseLoaderWithUrl: url_
                                        postData: post_data_ ];
}

-(JFFAsyncOperation)getSymbolsWithSid:( NSString* )sid_
                                count:( NSUInteger )count_
{
   NSURL* url_ = [ NSURL URLWithSid: sid_ ];

   NSString* post_format_ = @"{\"cmd\":\"getSymbols\",\"count\":\"%d\"}";
   NSString* post_        = [ NSString stringWithFormat: post_format_, count_ ];
   NSData*   post_data_   = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

    NSLog( @"getSymbols post: %@ url: %@", post_, url_ );

    return [ self validatedResponseLoaderWithUrl: url_
                                        postData: post_data_ ];
}

-(JFFAsyncOperation)doStepWithSid:( NSString* )sid_
                   symbsAndCoords:( NSArray* )step_
                            state:( MWDoStepState* )state_
{
    NSAssert( sid_, @"can not be empty" );

    NSURL* url_ = [ NSURL URLWithSid: sid_ ];

    NSString* post_ = nil;

    if ( step_ )
    {
       NSString* post_format_ = @"{\"cmd\":\"doStep\", %@, %@ }";
       post_ = [ NSString stringWithFormat: post_format_
                , [ step_ toJsonCommand ]
                , [ state_ toJsonCommand ] ];
    }
    else
    {
       NSString* post_format_ = @"{\"cmd\":\"doStep\", \"point\": \"%d\" }";
       post_ = [ NSString stringWithFormat: post_format_
                , state_.points ];
    }

    NSData* post_data_ = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

    NSLog( @"doStep post: %@ url: %@", post_, url_ );

    return [ self validatedResponseLoaderWithUrl: url_
                                        postData: post_data_ ];
}

-(JFFAsyncOperation)statisticForSid:( NSString* )sid_
{
    NSURL* url_ = [ NSURL URLWithSid: sid_ ];

    NSString* post_      = @"{\"cmd\":\"getStatistic\"}";
    NSData*   post_data_ = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

    NSLog( @"getStatistic post: %@ url: %@", post_, url_ );

    return [ self validatedResponseLoaderWithUrl: url_
                                        postData: post_data_ ];
}

@end
