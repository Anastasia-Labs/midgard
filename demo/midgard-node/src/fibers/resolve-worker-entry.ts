import { existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

export const resolveWorkerEntry = (
  moduleUrl: string,
  workerFileName: string,
): URL => {
  const moduleDir = dirname(fileURLToPath(moduleUrl));
  const candidates = [
    join(moduleDir, workerFileName),
    join(moduleDir, "..", "workers", workerFileName),
    join(moduleDir, "..", workerFileName),
    join(moduleDir, "..", "..", "dist", workerFileName),
  ];

  for (const candidate of candidates) {
    if (existsSync(candidate)) {
      return pathToFileURL(candidate);
    }
  }

  throw new Error(
    `Failed to resolve worker entry ${workerFileName} from ${moduleDir}`,
  );
};
