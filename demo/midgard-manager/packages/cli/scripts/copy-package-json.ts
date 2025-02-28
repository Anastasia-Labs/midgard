import { readFileSync, writeFileSync } from 'fs';
import { join } from 'path';

const packageJson = JSON.parse(readFileSync('package.json', 'utf8'));

const newPackageJson = {
  ...packageJson,
  main: './bin.js',
  types: './bin.d.ts',
  scripts: undefined,
  devDependencies: undefined,
};

writeFileSync(join('dist', 'package.json'), JSON.stringify(newPackageJson, null, 2));
