package entity

import "github.com/prometheus/client_golang/prometheus"

type MetricsOpts struct {
	Namespace string
	Subsystem string

	EnableAll           bool
	EnableSnapshotTimer bool
}

type Metrics struct {
	Timer *prometheus.HistogramVec
}

func (m *Metrics) Slice() []prometheus.Collector {
	cs := make([]prometheus.Collector, 0, 1)
	if c := m.Timer; c != nil {
		cs = append(cs, c)
	}
	return cs
}

func BuildMetrics(opts *MetricsOpts) Metrics {
	metrics := Metrics{}
	if opts.EnableAll {
		opts.EnableSnapshotTimer = true
	}
	if opts.EnableSnapshotTimer {
		metrics.Timer = prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name:      "timer_seconds",
			Namespace: opts.Namespace,
			Subsystem: opts.Subsystem,
			Buckets:   []float64{0.001, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 100},
		}, []string{"step"})
	}
	return metrics
}

func GetMetricsOpts() *MetricsOpts {
	return &MetricsOpts{
		Namespace: "midgard",
		Subsystem: "node",

		EnableAll: true,
	}
}
