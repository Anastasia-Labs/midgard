package cmd

import (
	"context"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/Anastasia-Labs/midgard-node-go/config"
	"github.com/Anastasia-Labs/midgard-node-go/controller"
	. "github.com/Anastasia-Labs/midgard-node-go/entity"
	"github.com/gin-contrib/pprof"
	"github.com/gin-gonic/gin"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/spf13/cobra"
	"go.uber.org/zap"
)

var NodeCmd = &cobra.Command{
	Use:     "node",
	Short:   "node - node service",
	Long:    `node service`,
	Version: "v0.0.1",
	Run: func(cmd *cobra.Command, args []string) {
		cfg, err := config.NewNodeConfig()
		if err != nil {
			log.Panic("Read config error: ", err.Error())
		}
		var l *zap.Logger
		if cfg.LogLevel == "debug" {
			l, err = zap.NewDevelopment()
		} else {
			l, err = zap.NewProduction()
		}
		if err != nil {
			log.Panic("Create logger error: ", err)
		}
		logger := l.Sugar()
		defer func() {
			logger.Sync()
		}()
		logger.Info("Read node config success!!!")
		logger.Debug("Create node controller...")
		ctx := context.Background()
		nodectl, err := controller.NewNodeController(cfg.Peers, cfg.CryptoKeyPath, ctx, logger)
		restAPIRouter := gin.Default()
		pprof.Register(restAPIRouter)
		metrics := nodectl.RegisterMetrics(GetMetricsOpts())
		registerMetrics(restAPIRouter, metrics)
		restAPIRouter.GET("/healthz", func(c *gin.Context) {
			c.JSON(http.StatusOK, gin.H{
				"message": "success",
			})
		})
		v1 := restAPIRouter.Group("/v1")
		{
			v1.POST("/tx", nodectl.SubmitTx)
			v1.GET("/mempool", nodectl.GetMempool)
		}
		server := &http.Server{
			Addr:    ":8080",
			Handler: restAPIRouter,
		}
		go func() {
			if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
				logger.Panic("Failed to start server: ", err)
			}
		}()
		logger.Info("Node running...")
		sigusr := make(chan os.Signal, 1)
		signal.Notify(sigusr, syscall.SIGUSR1)
		sigterm := make(chan os.Signal, 1)
		signal.Notify(sigterm, syscall.SIGINT, syscall.SIGTERM)
		select {
		case <-sigterm:
			logger.Info("server: terminating by signal")
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			if err := server.Shutdown(ctx); err != nil {
				logger.Fatalf("Server shutdown failed: %+v", err)
			}
			logger.Info("Server gracefully stopped")
		case <-sigusr:
			logger.Info("server: pause message consumption")
		}
		nodectl.Stop()
		logger.Info("Node stopped ...")
	}}

func registerMetrics(g *gin.Engine, metrics *Metrics) {
	prometheus.MustRegister()
	prometheus.MustRegister(metrics.Slice()...)
	handler := promhttp.Handler()
	g.GET("/metrics", func(c *gin.Context) {
		handler.ServeHTTP(c.Writer, c.Request)
	})
}
