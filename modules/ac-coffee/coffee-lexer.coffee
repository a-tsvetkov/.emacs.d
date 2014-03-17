#!/usr/bin/env coffee

# Get coffee
coffee = require 'coffee-script/lib/coffee-script/lexer'
fs = require 'fs'

# Get lexer
lexer = new coffee.Lexer()

# Check arguments
# should path the full path to source
argn = process.argv.length

file_path = ""
if argn != 3
        console.error "Lexer: No file name"
        process.exit(1)
else
        file_path = process.argv[2]


try
        code = fs.readFileSync(file_path, 'utf-8')
catch error
        console.error "Lexer: #{error}"
        process.exit(1)

# Get all identifiers
try
        ID = []
        for token in lexer.tokenize(code)
                if token[0] == "IDENTIFIER"
                        if token[1] not in ID
                                ID.push token[1]
catch error
        console.error "Lexer: #{error}"
        process.exit(1)
        
# output
for id in ID
        console.log id
        
        
