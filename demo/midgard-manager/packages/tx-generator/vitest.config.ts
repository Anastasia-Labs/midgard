import tsconfigPaths from 'vite-tsconfig-paths';
import { defineConfig } from 'vitest/config';

export default defineConfig({
  plugins: [tsconfigPaths()],
  test: {
    reporters: 'verbose',
    include: ['./tests/**/*.test.{js,mjs,cjs,ts,mts,cts,jsx,tsx}'],
    testTimeout: 60_000, // 60 seconds timeout
    bail: 3,
  },
});
