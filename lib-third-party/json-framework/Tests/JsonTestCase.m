/*
 Copyright (C) 2011 Stig Brautaset. All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 
 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.
 
 * Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the
 documentation and/or other materials provided with the distribution.
 
 * Neither the name of the author nor the names of its contributors
 may be used to endorse or promote products derived from this
 software without specific prior written permission.
 
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 
 */

#import "JsonTestCase.h"

@interface JsonTestCase ()

@property ( nonatomic, retain ) SBJsonWriter * writer;
@property ( nonatomic, retain ) SBJsonParser * parser;

@end

@implementation JsonTestCase

@synthesize writer = _writer;
@synthesize parser = _parser;

-(void)dealloc
{
   [ _writer release ];
   [ _parser release ];

   [ super dealloc ];
}

- (void)setUp {
    count = 0;
    self.parser = [[[SBJsonParser alloc] init]autorelease];
    self.writer = [[[SBJsonWriter alloc] init]autorelease];
}

- (NSString*)otherFileName {
    return @"output";
}

- (void)foreachTestInSuite:(NSString*)suite apply:(void(^)(NSString*, NSString*))block {
    NSString *file;
    NSDirectoryEnumerator* enumerator = [[NSFileManager defaultManager] enumeratorAtPath:suite];
    while ((file = [enumerator nextObject])) {
        NSString *path = [suite stringByAppendingPathComponent:file];
        NSString *inpath = [path stringByAppendingPathComponent:@"input"];

        if ([[NSFileManager defaultManager] isReadableFileAtPath:inpath]) {
            NSString *outpath = [path stringByAppendingPathComponent:[self otherFileName]];
            STAssertTrue([[NSFileManager defaultManager] isReadableFileAtPath:outpath], nil);
            block(inpath, outpath);
            count++;
        }
    }
}
@end
