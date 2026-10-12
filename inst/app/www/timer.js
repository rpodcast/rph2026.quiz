// Quiz countdown. The server owns the deadline; this only displays it and
// notifies the server when it reaches zero.
(function () {
  var state = { interval: null, endAt: null, inputId: null, fired: false };

  function el() {
    return document.getElementById("quiz-timer");
  }

  function format(ms) {
    var total = Math.max(0, Math.ceil(ms / 1000));
    var m = Math.floor(total / 60);
    var s = total % 60;
    return m + ":" + (s < 10 ? "0" : "") + s;
  }

  function tick() {
    var remaining = state.endAt - Date.now();
    var node = el();
    if (node) {
      node.textContent = format(remaining);
      node.classList.toggle("text-bg-secondary", remaining > 60000);
      node.classList.toggle("text-bg-warning", remaining <= 60000 && remaining > 15000);
      node.classList.toggle("text-bg-danger", remaining <= 15000);
    }
    if (remaining <= 0 && !state.fired) {
      state.fired = true;
      Shiny.setInputValue(state.inputId, Date.now(), { priority: "event" });
    }
  }

  function stop() {
    if (state.interval) {
      clearInterval(state.interval);
      state.interval = null;
    }
    var node = el();
    if (node) {
      node.classList.add("d-none");
    }
  }

  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("quiz_timer_start", function (msg) {
      stop();
      state.inputId = msg.input_id;
      state.endAt = Date.now() + msg.remaining_ms;
      state.fired = false;
      var node = el();
      if (node) {
        node.classList.remove("d-none");
        node.setAttribute("aria-label", "Time remaining");
      }
      tick();
      state.interval = setInterval(tick, 250);
    });

    // Shiny requires handlers to declare exactly one argument
    Shiny.addCustomMessageHandler("quiz_timer_stop", function (msg) {
      stop();
    });

    // Numbered question navigator; clicking sends the 1-based index
    Shiny.addCustomMessageHandler("quiz_nav", function (msg) {
      var container = document.getElementById(msg.id);
      if (!container) {
        return;
      }
      var answered = [].concat(msg.answered);
      if (container.children.length !== answered.length) {
        container.innerHTML = "";
        answered.forEach(function (_, i) {
          var btn = document.createElement("button");
          btn.type = "button";
          btn.className = "btn btn-sm quiz-nav-btn";
          btn.textContent = String(i + 1);
          btn.dataset.index = String(i + 1);
          container.appendChild(btn);
        });
        container.onclick = function (event) {
          var btn = event.target.closest(".quiz-nav-btn");
          if (btn) {
            Shiny.setInputValue(container.dataset.inputId, Number(btn.dataset.index), {
              priority: "event"
            });
          }
        };
      }
      container.dataset.inputId = msg.input_id;
      Array.prototype.forEach.call(container.children, function (btn, i) {
        var isAnswered = answered[i] === true;
        var isCurrent = i + 1 === msg.current;
        btn.classList.toggle("btn-primary", isAnswered);
        btn.classList.toggle("btn-outline-primary", !isAnswered);
        btn.classList.toggle("quiz-nav-current", isCurrent);
        if (isCurrent) {
          btn.setAttribute("aria-current", "step");
        } else {
          btn.removeAttribute("aria-current");
        }
        btn.setAttribute(
          "aria-label",
          "Question " + (i + 1) + (isAnswered ? ", answered" : ", unanswered")
        );
      });
    });

    Shiny.addCustomMessageHandler("quiz_progress", function (msg) {
      var bar = document.getElementById(msg.id);
      if (bar) {
        bar.style.width = msg.percent + "%";
      }
    });
  });
})();
