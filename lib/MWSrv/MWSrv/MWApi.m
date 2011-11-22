#import "MWApi.h"

#import "NSString+RandomString.h"

static NSUInteger sidLength_ = 32;
static NSString* const host_format_ = @"http://test.bwf.org.ua:3333/%@";

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
      id< JNUrlResponse > response_ = result_;
      if ( response_ )
      {
         if ( 200 == response_.statusCode )
         {
            done_callback_( sid_, nil );
         }
         else
         {
            NSString* error_format_ = @"Invalid response code: %d";
            NSString* error_description_ = [ NSString stringWithFormat:
                                            error_format_
                                            , response_.statusCode ];
            done_callback_( nil, [ JFFError errorWithDescription: error_description_ ] );
         }
      }
      else
      {
         done_callback_( nil, error_ );
      }
   };

   return asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
}

-(JFFAsyncOperation)createGameWithName:( NSString* )name_
                                   sid:( NSString* )sid_
{
   NSURL* url_ = [ NSURL URLWithSid: sid_ ];

   NSString* post_format_ = @"{\"cmd\":\"createGame\",\"name\":\"%@\"}";
   NSString* post_        = [ NSString stringWithFormat: post_format_, name_ ? name_ : @"" ];
   NSData*   post_data_   = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

   JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                        , post_data_
                                                        , self.headers );

   JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^void( id result_
                                                                , NSError* error_
                                                                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      id< JNUrlResponse > response_ = result_;
      if ( response_ )
      {
         if ( 200 == response_.statusCode )
         {
            done_callback_( [ NSNull null ], nil );
         }
         else
         {
            NSString* error_format_ = @"Invalid response code: %d";
            NSString* error_description_ = [ NSString stringWithFormat:
                                            error_format_
                                            , response_.statusCode ];
            done_callback_( nil, [ JFFError errorWithDescription: error_description_ ] );
         }
      }
      else
      {
         done_callback_( nil, error_ );
      }
   };

   return asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
}

-(JFFAsyncOperation)getListOfGamesForSid:( NSString* )sid_
{
   NSURL* url_ = [ NSURL URLWithSid: sid_ ];

   NSString* post_      = @"{\"cmd\":\"getListOfGames\"}";
   NSData*   post_data_ = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

   JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                        , post_data_
                                                        , self.headers );

   JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^void( id result_
                                                                , NSError* error_
                                                                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      id< JNUrlResponse > response_ = result_;
      if ( response_ )
      {
         if ( 200 == response_.statusCode )
         {
            done_callback_( [ NSNull null ], nil );
         }
         else
         {
            NSString* error_format_ = @"Invalid response code: %d";
            NSString* error_description_ = [ NSString stringWithFormat:
                                            error_format_
                                            , response_.statusCode ];
            done_callback_( nil, [ JFFError errorWithDescription: error_description_ ] );
         }
      }
      else
      {
         done_callback_( nil, error_ );
      }
   };

   return asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
}

@end
