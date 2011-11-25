//
//  AppDelegate.m
//  testSrv
//
//  Created by Vladimir on 21.11.11.
//  Copyright (c) 2011 __MyCompanyName__. All rights reserved.
//

#import "AppDelegate.h"

#import "ViewController.h"

#import <MWSrv/MWSession.h>

@interface AppDelegate ()

@property ( nonatomic, retain ) MWSession* session1;
@property ( nonatomic, retain ) MWSession* session2;

@end

@implementation AppDelegate

@synthesize window = _window;
@synthesize viewController = _viewController;
@synthesize session1 = _session1;
@synthesize session2 = _session2;

- (void)dealloc
{
   [ _window release ];
   [ _viewController release ];
   [ _session1 release ];
   [ _session2 release ];

   [ super dealloc ];
}

-(void)loginAndPlayPlayer2
{
   self.session2 = [ MWSession sessionWithLogin: @"testUser2" ];

   [ self.session2 playBattleground ]( nil, nil, ^( id result_, NSError* error_ )
   {
      NSLog( @"result2: %@ error: %@", result_, error_ );
   } );
}

-(void)test
{
   self.session1 = [ MWSession sessionWithLogin: @"testUser1" ];

   [ self.session1 playBattleground ]( nil, nil, ^( id result_, NSError* error_ )
   {
      NSLog( @"result1: %@ error: %@", result_, error_ );
   } );

   [ self performSelector: @selector( loginAndPlayPlayer2 )
               withObject: nil
               afterDelay: 2.0 ];
}

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    self.window = [[[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]] autorelease];
    // Override point for customization after application launch.
   self.viewController = [[[ViewController alloc] initWithNibName:@"ViewController" bundle:nil] autorelease];
   self.window.rootViewController = self.viewController;
   [ self.window makeKeyAndVisible ];

   [ self test ];

   return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application
{
   /*
    Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
    */
}

- (void)applicationDidEnterBackground:(UIApplication *)application
{
   /*
    Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
    If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    */
}

- (void)applicationWillEnterForeground:(UIApplication *)application
{
   /*
    Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
    */
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
   /*
    Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
    */
}

- (void)applicationWillTerminate:(UIApplication *)application
{
   /*
    Called when the application is about to terminate.
    Save data if appropriate.
    See also applicationDidEnterBackground:.
    */
}

@end
