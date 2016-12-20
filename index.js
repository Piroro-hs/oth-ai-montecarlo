const {exec} = require('child_process');
// const {cpus} = require('os');

module.exports = function montecarlohs({B, W, toArray}) {
  const parse = function parse(piece) {
    return piece === B ? 'B' : piece === W ? 'W' : 'E';
  };
  return {
    compute({history: [{board, turn}]}) {
      return new Promise((resolve) => {
        const boardStr = JSON.stringify(toArray(board).map(parse)).replace(/"/g, '\\"');
        exec(
          // `montecarlo-exe ${boardStr} ${parse(turn)} 1000 +RTS -N${cpus().length - 2}`,
          `montecarlo-exe ${boardStr} ${parse(turn)} 1000`,
          {cwd: __dirname},
          (_, out) => {
            const [, row, column] = out.match(/\((\d+),(\d+)\)/);
            resolve({row: parseInt(row, 10), column: parseInt(column, 10)});
          }
        );
      });
    },
  };
};
