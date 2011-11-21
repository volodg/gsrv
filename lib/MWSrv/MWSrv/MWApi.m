#import "MWApi.h"

#import "NSString+RandomString.h"

static NSUInteger sidLength_ = 32;
static NSString* const host_format_ = @"http://test.bwf.org.ua:3333/%@";

@interface MWApi ()
@end

@implementation MWApi

-(JFFAsyncOperation)authLoaderWithLogin:( NSString* )login_
{
   NSString* sid_ = [ NSString randomStringWithLength: sidLength_ ];
   NSString* str_ = [ NSString stringWithFormat: host_format_, sid_ ];
   NSURL*    url_ = [ NSURL URLWithString: str_ ];

   NSString* post_format_ = @"{\"cmd\":\"enter\",\"env\":1,\"cid\":\"%@\"}";
   NSString* post_        = [ NSString stringWithFormat: post_format_, login_ ? login_ : @"" ];
   NSData*   post_data_   = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

   NSDictionary* headers_ = [ NSDictionary dictionaryWithObjectsAndKeys:
                             @"application/json", @"Content-type"
                             , nil ];

   JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                        , post_data_
                                                        , headers_ );

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

@end
