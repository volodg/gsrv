//
//  ViewController.h
//  testSrv
//
//  Created by Vladimir on 21.11.11.
//  Copyright (c) 2011 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface ViewController : UIViewController

@property (retain, nonatomic) IBOutlet UITextField *firstLetter;
@property (retain, nonatomic) IBOutlet UITextField *secondLetter;

- (IBAction)doStep1:(id)sender;
- (IBAction)doStep2:(id)sender;


@end
