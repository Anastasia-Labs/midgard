import { main } from "./commit-paths.main.mjs";
import { Stop } from "./commit-paths.parse-args.mjs";

try {
  process.exitCode = main(process.argv.slice(2));
} catch (error) {
  if (error instanceof Stop) {
    process.stderr.write(`commit-paths: ${error.message}\n`);
    process.exitCode = error.code;
  } else {
    throw error;
  }
}
