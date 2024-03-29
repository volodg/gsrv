/*
 Copyright (C) 2010 Stig Brautaset. All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
 
 * Redistributions of source code must retain the above copyright notice, this
 list of conditions and the following disclaimer.
 
 * Redistributions in binary form must reproduce the above copyright notice,
 this list of conditions and the following disclaimer in the documentation
 and/or other materials provided with the distribution.
 
 * Neither the name of the author nor the names of its contributors may be used
 to endorse or promote products derived from this software without specific
 prior written permission.
 
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
 FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#import <SenTestingKit/SenTestingKit.h>
#import <SBJson/SBJson.h>

@interface WriterTest : SenTestCase {
	SBJsonWriter * _writer;
}

@property ( nonatomic, retain ) SBJsonWriter * writer;

@end

@implementation WriterTest

@synthesize writer = _writer;

- (void)setUp {
    self.writer = [ [SBJsonWriter new] autorelease];
}

- (void)testInfinity {
    STAssertEqualObjects(@"[0]", [writer stringWithObject:[NSArray arrayWithObject:[NSNumber numberWithDouble:0.0]]], nil);
    STAssertEqualObjects(@"[-0]", [writer stringWithObject:[NSArray arrayWithObject:[NSNumber numberWithDouble:-0.0]]], nil);
    
    STAssertEqualObjects(@"[0]", [writer stringWithObject:[NSArray arrayWithObject:[NSNumber numberWithFloat:(float)0.0]]], nil);
    STAssertEqualObjects(@"[-0]", [writer stringWithObject:[NSArray arrayWithObject:[NSNumber numberWithFloat:(float)-0.0]]], nil);
    
    STAssertEqualObjects(@"[0]", [writer stringWithObject:[NSArray arrayWithObject:[NSNumber numberWithInt:0]]], nil);
    STAssertEqualObjects(@"[0]", [writer stringWithObject:[NSArray arrayWithObject:[NSNumber numberWithInt:-0]]], nil);
			 
    STAssertEqualObjects(@"[0]", [writer stringWithObject:[NSArray arrayWithObject:[NSDecimalNumber numberWithDouble:0]]], nil);
    STAssertEqualObjects(@"[0]", [writer stringWithObject:[NSArray arrayWithObject:[NSDecimalNumber numberWithDouble:-0]]], nil);
	
    STAssertEqualObjects(@"[0]", [writer stringWithObject:[NSArray arrayWithObject:[NSDecimalNumber numberWithFloat:0]]], nil);
    STAssertEqualObjects(@"[0]", [writer stringWithObject:[NSArray arrayWithObject:[NSDecimalNumber numberWithFloat:-0]]], nil);    
}

- (void)testTimeInterval {
	NSTimeInterval interval = 319670801.45073098; // seconds since epoc
	NSNumber *number = [NSNumber numberWithDouble:interval];
	NSArray *array = [NSArray arrayWithObject:number];

	STAssertEqualObjects(@"[319670801.45073098]", [writer stringWithObject:array], nil);
}


- (void)testWriteToStream {
	SBJsonStreamWriter *streamWriter = [[[SBJsonStreamWriter alloc] init]autorelease];
	
	STAssertTrue([streamWriter writeArray:[NSArray array]], nil);
	
	STAssertFalse([streamWriter writeArray:[NSArray array]], nil);
	STAssertEqualObjects(streamWriter.error, @"Stream is closed", nil);
}

@end
