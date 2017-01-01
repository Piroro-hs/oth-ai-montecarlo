const {execSync: exec} = require('child_process');
const {basename, parse} = require('path');

const {copySync: copy, walkSync: walk} = require('fs-extra');
const {sync: hasbin} = require('hasbin');

if (!hasbin('stack')) {
  console.error(new Error('Stack not found.'));
  process.exit(1);
}

exec('stack build', {cwd: 'lib', stdio: 'inherit'});

const [libPath] = walk('lib/.stack-work/install').filter(p => parse(p).name === 'montecarlo-exe');

copy(libPath, basename(libPath));
