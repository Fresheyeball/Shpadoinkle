import {h} from './h.js';
import {init} from './init.js';
import {classModule} from './modules/class.js';
import {propsModule} from './modules/props.js';
import {attributesModule} from './modules/attributes.js';
import {eventListenersModule} from './modules/eventlisteners.js';


window.startApp = cb => {
    const patch = init([
        propsModule,
        classModule,
        attributesModule,
        eventListenersModule
    ]);
    window.patchh = (a,b) => patch(a,b);
    window.vnode = h;
    window.potato = (n,e) => n.elm.appendChild(e);
    window.insertHook = (k,f,o) => {
      const p = o[k]
      o[k] = (vnode) => {
          if(p){ p(vnode); }
          f(vnode);
      }
    };

    cb();
};
