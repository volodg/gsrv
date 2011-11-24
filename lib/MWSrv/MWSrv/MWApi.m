#import "MWApi.h"

#import "NSString+RandomString.h"

#import "JMonad.h"

#import <AMFUnarchiver.h>

static NSUInteger sidLength_ = 32;
static NSString* const host_format_ = @"http://test.bwf.org.ua:3333/%@";

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
   id value_ = [ NSNull null ];
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
{
   NSString* sid_ = [ NSString randomStringWithLength: sidLength_ ];
   NSURL*    url_ = [ NSURL URLWithSid: sid_ ];

   NSString* post_format_ = @"{\"cmd\":\"enter\",\"env\":1,\"cid\":\"%@\"}";
   NSString* post_        = [ NSString stringWithFormat: post_format_, login_ ? login_ : @"" ];
   NSData*   post_data_   = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

   JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                        , post_data_
                                                        , self.headers );

   JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^void( id result_
                                                                , NSError* error_
                                                                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      JEitherMonad* monad_ = [ [ self monadForResponse: result_ error: error_ ]
      bindVoidOperation: ^id<JMonad>{
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

         [ monad_ notifyDoneBlock: done_callback_ ];
      };

      progress_callback_ = [ [ progress_callback_ copy ] autorelease ];
      progress_callback_ = ^( id progress_data_ )
      {
         [ response_data_ appendData: progress_data_ ];
         if ( progress_callback_ )
            progress_callback_( progress_data_ );
      };

      loader_ = asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
      return loader_( progress_callback_
                     , cancel_callback_
                     , done_callback_  );
   } copy ] autorelease ];
}

@end
