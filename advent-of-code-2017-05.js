// jshint esversion: 6
const fs = require('fs');

function parseAndThen(whatToDo) {
  return (err, data) => {
    instructions = data.trim();
    instructions = instructions.split('\n').map(x => parseInt(x, 10));
    return whatToDo(instructions);
  };
}

function answerFirstQuestion(instructions) {
  executeWithIncrement(instructions, (instruction) => instruction + 1, "first question: %d");
}

function answerSecondQuestion(instructions) {
  executeWithIncrement(instructions, (instruction) => instruction >=3 ? -1 : 1, "second question: %d");
}

function executeWithIncrement(instructions, increment, label) {
  var count = 0;
  var offset = 0;
  while(offset >= 0 && offset < instructions.length) {
    var toffset = offset;
    offset += instructions[offset];
    instructions[toffset] += increment(instructions[toffset]);
    count++;
  }
  console.log(label, count);
}

fs.readFile(__dirname + '/advent-of-code-2017-05.data', 'utf8',
            parseAndThen(answerFirstQuestion));

fs.readFile(__dirname + '/advent-of-code-2017-05.data', 'utf8',
            parseAndThen(answerSecondQuestion));
