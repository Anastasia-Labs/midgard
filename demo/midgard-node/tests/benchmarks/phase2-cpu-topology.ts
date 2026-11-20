import { execFile } from "node:child_process";
import { readFile } from "node:fs/promises";
import { availableParallelism, hostname } from "node:os";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);

const expandCpuList = (cpuList: string): readonly number[] =>
  cpuList
    .trim()
    .split(",")
    .flatMap((part) => {
      const [startText, endText] = part.split("-");
      const start = Number(startText);
      const end = endText === undefined ? start : Number(endText);
      if (
        !Number.isSafeInteger(start) ||
        !Number.isSafeInteger(end) ||
        end < start
      ) {
        throw new Error(`Invalid CPU affinity range ${JSON.stringify(part)}`);
      }
      return Array.from(
        { length: end - start + 1 },
        (_, offset) => start + offset,
      );
    });

export const readPhase2CpuTopology = async (): Promise<{
  readonly logicalCpuIds: readonly number[];
  readonly physicalCoreIds: readonly string[];
  readonly pinnedEightCore: boolean;
}> => {
  const status = await readFile("/proc/self/status", "utf8");
  const allowedList = /^Cpus_allowed_list:\s*(.+)$/mu.exec(status)?.[1];
  if (allowedList === undefined) {
    throw new Error("Unable to read Cpus_allowed_list from /proc/self/status");
  }
  const logicalCpuIds = expandCpuList(allowedList);
  const physicalCoreIds = [
    ...new Set(
      await Promise.all(
        logicalCpuIds.map(async (cpuId) => {
          const topologyRoot = `/sys/devices/system/cpu/cpu${cpuId}/topology`;
          const [packageId, coreId] = await Promise.all([
            readFile(`${topologyRoot}/physical_package_id`, "utf8"),
            readFile(`${topologyRoot}/core_id`, "utf8"),
          ]);
          return `${packageId.trim()}:${coreId.trim()}`;
        }),
      ),
    ),
  ].sort();
  return {
    logicalCpuIds,
    physicalCoreIds,
    pinnedEightCore:
      availableParallelism() === 8 &&
      logicalCpuIds.length === 8 &&
      physicalCoreIds.length === 8,
  };
};

export const readPhase2ContainerIdentity = async (
  expectedImage: string,
  expectedLogicalCpuIds: readonly number[],
): Promise<{
  readonly proved: boolean;
  readonly image: string;
  readonly imageId: string;
  readonly id: string;
}> => {
  const { stdout } = await execFileAsync("docker", ["inspect", hostname()]);
  const inspected = (
    JSON.parse(stdout) as readonly {
      readonly Id: string;
      readonly Image: string;
      readonly Config: { readonly Image: string; readonly Hostname: string };
      readonly HostConfig: {
        readonly AutoRemove: boolean;
        readonly CpusetCpus: string;
      };
      readonly State: { readonly Running: boolean };
      readonly NetworkSettings: {
        readonly Ports: Record<
          string,
          readonly { readonly HostPort: string }[] | null
        >;
      };
    }[]
  )[0];
  if (inspected === undefined) {
    throw new Error(
      "Docker inspect returned no record for benchmark container",
    );
  }
  const logicalCpuIds = expandCpuList(inspected.HostConfig.CpusetCpus);
  const sameCpuIds =
    logicalCpuIds.length === expectedLogicalCpuIds.length &&
    logicalCpuIds.every(
      (cpuId, index) => cpuId === expectedLogicalCpuIds[index],
    );
  const publishedPorts = Object.values(inspected.NetworkSettings.Ports).flatMap(
    (bindings) => bindings ?? [],
  );
  return {
    proved:
      inspected.State.Running &&
      inspected.HostConfig.AutoRemove &&
      inspected.Config.Image === expectedImage &&
      /^sha256:[0-9a-f]{64}$/u.test(inspected.Image) &&
      (inspected.Id.startsWith(hostname()) ||
        inspected.Config.Hostname === hostname()) &&
      sameCpuIds &&
      publishedPorts.length === 0,
    image: inspected.Config.Image,
    imageId: inspected.Image,
    id: inspected.Id,
  };
};
