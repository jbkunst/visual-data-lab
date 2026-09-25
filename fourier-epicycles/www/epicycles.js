(() => {
  const state = {
    components: [],
    original: [],
    speed: 1,
    playing: true,
    showCircles: true,
    showVectors: true,
    showOriginal: true,
    showTrace: true,
    time: 0,
    trace: [],
    lastFrame: performance.now()
  };

  let canvas;
  let context;
  let resizeObserver;

  function colors() {
    const style = getComputedStyle(document.documentElement);

    return {
      primary: style.getPropertyValue("--bs-primary").trim() || "#0d6efd",
      body: style.getPropertyValue("--bs-body-color").trim() || "#212529",
      muted: style.getPropertyValue("--bs-secondary-color").trim() || "#6c757d",
      border: style.getPropertyValue("--bs-border-color").trim() || "#dee2e6",
      background: style.getPropertyValue("--bs-body-bg").trim() || "#ffffff"
    };
  }

  function resizeCanvas() {
    if (!canvas) return;

    const bounds = canvas.getBoundingClientRect();
    const ratio = window.devicePixelRatio || 1;

    canvas.width = Math.max(1, Math.floor(bounds.width * ratio));
    canvas.height = Math.max(1, Math.floor(bounds.height * ratio));
    context.setTransform(ratio, 0, 0, ratio, 0, 0);
  }

  function reset() {
    state.time = 0;
    state.trace = [];
    state.lastFrame = performance.now();
  }

  function transformPoint(x, y, scale, centerX, centerY) {
    return {
      x: centerX + scale * x,
      y: centerY - scale * y
    };
  }

  function drawPath(points, scale, centerX, centerY, stroke, width, close = true) {
    if (!points.length) return;

    context.beginPath();

    points.forEach((point, index) => {
      const p = transformPoint(point.x, point.y, scale, centerX, centerY);
      if (index === 0) context.moveTo(p.x, p.y);
      else context.lineTo(p.x, p.y);
    });

    if (close) context.closePath();
    context.strokeStyle = stroke;
    context.lineWidth = width;
    context.stroke();
  }

  function endpointAndChain() {
    const chain = [];
    let x = 0;
    let y = 0;

    state.components.forEach(component => {
      const previousX = x;
      const previousY = y;
      const angle = component.frequency * state.time + component.phase;

      x += component.amplitude * Math.cos(angle);
      y += component.amplitude * Math.sin(angle);

      chain.push({
        x: previousX,
        y: previousY,
        radius: component.amplitude,
        endX: x,
        endY: y
      });
    });

    return { x, y, chain };
  }

  function drawFrame(now) {
    if (!canvas || !context) {
      requestAnimationFrame(drawFrame);
      return;
    }

    const width = canvas.clientWidth;
    const height = canvas.clientHeight;
    const palette = colors();

    context.clearRect(0, 0, width, height);
    context.fillStyle = palette.background;
    context.fillRect(0, 0, width, height);

    const radii = state.components.reduce((sum, x) => sum + x.amplitude, 0);
    const extent = Math.max(1.25, Math.min(2.5, radii));
    const scale = 0.42 * Math.min(width, height) / extent;
    const centerX = width / 2;
    const centerY = height / 2;

    if (state.showOriginal) {
      drawPath(state.original, scale, centerX, centerY, palette.border, 2);
    }

    const current = endpointAndChain();

    if (state.showCircles) {
      current.chain.forEach(item => {
        const center = transformPoint(item.x, item.y, scale, centerX, centerY);

        context.beginPath();
        context.arc(center.x, center.y, item.radius * scale, 0, 2 * Math.PI);
        context.strokeStyle = palette.border;
        context.lineWidth = 1;
        context.stroke();
      });
    }

    if (state.showVectors) {
      current.chain.forEach(item => {
        const start = transformPoint(item.x, item.y, scale, centerX, centerY);
        const end = transformPoint(item.endX, item.endY, scale, centerX, centerY);

        context.beginPath();
        context.moveTo(start.x, start.y);
        context.lineTo(end.x, end.y);
        context.strokeStyle = palette.muted;
        context.lineWidth = 1.5;
        context.stroke();
      });
    }

    if (state.showTrace && state.trace.length > 1) {
      drawPath(state.trace, scale, centerX, centerY, palette.primary, 2.5, false);
    }

    const tip = transformPoint(current.x, current.y, scale, centerX, centerY);
    context.beginPath();
    context.arc(tip.x, tip.y, 3.5, 0, 2 * Math.PI);
    context.fillStyle = palette.primary;
    context.fill();

    const elapsed = Math.min(now - state.lastFrame, 100);
    state.lastFrame = now;

    if (state.playing && state.components.length) {
      state.time += elapsed * state.speed * 2 * Math.PI / 6000;

      if (state.time >= 2 * Math.PI) {
        state.time %= 2 * Math.PI;
        state.trace = [];
      }

      state.trace.push({ x: current.x, y: current.y });
      if (state.trace.length > 2000) state.trace.shift();
    }

    requestAnimationFrame(drawFrame);
  }

  function registerHandlers() {
    if (!window.Shiny) {
      window.setTimeout(registerHandlers, 50);
      return;
    }

    Shiny.addCustomMessageHandler("epicycles-data", message => {
      state.components = message.components || [];
      state.original = message.path || [];
      reset();
    });

    Shiny.addCustomMessageHandler("epicycles-options", message => {
      Object.assign(state, message);
    });

    Shiny.addCustomMessageHandler("epicycles-command", message => {
      if (message.command === "restart") reset();
    });
  }

  function initialize() {
    canvas = document.getElementById("epicycle-canvas");
    if (!canvas) {
      window.setTimeout(initialize, 50);
      return;
    }

    context = canvas.getContext("2d");
    resizeObserver = new ResizeObserver(resizeCanvas);
    resizeObserver.observe(canvas);
    resizeCanvas();
    requestAnimationFrame(drawFrame);
  }

  registerHandlers();

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialize);
  } else {
    initialize();
  }
})();
