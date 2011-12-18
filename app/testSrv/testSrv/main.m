//
//  main.m
//  testSrv
//
//  Created by Vladimir on 21.11.11.
//  Copyright (c) 2011 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

#import "AppDelegate.h"

int main(int argc, char *argv[])
{
	NSAutoreleasePool* pool = [ NSAutoreleasePool new ];
    int res = UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));
	[ pool release ];
	return res;
}
