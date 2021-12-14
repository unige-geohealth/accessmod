/**
 * Simple modal
 */

const modalDefault = {
  idTarget: 'amModal',
  title: 'Modal',
  content: 'Hello',
  subtitle: null,
  buttons: null,
  addBackground: true
};

function buildModal(opt) {
  opt = Object.assign({}, modalDefault, opt);

  const el = El.el; // el module
  const elTarget = document.getElementById(opt.idTarget);

  const elClose = el(
    'button',
    {on: ['click', close], class: ['btn', 'btn-modal']},
    'Close'
  );
  if (!opt.buttons) {
    opt.buttons = [];
  }

  opt.buttons.push(elClose);

  const elModal = el(
    'div',
    {
      class: ['panel-modal-content', 'col-xs-12', 'col-sm-6', 'col-lg-4']
    },
    [
      el(
        'a',
        {
          href: '#',
          on: ['click', close],
          style: {
            float: 'right',
            color: 'black'
          }
        },
        [el('i', {class: ['fa', 'fa-times']})]
      ),
      el('div', {class: 'panel-modal-head'}, [
        el('div', {class: 'panel-modal-title'}, opt.title)
      ]),
      el('div', {class: 'panel-modal-subtitle'}, opt.subtitle),
      el('div', {class: 'panel-modal-container'}, [
        el('div', {class: 'pane-modal-text'}, opt.content)
      ]),
      el('div', {class: 'panel-modal-buttons'}, opt.buttons)
    ]
  );
  const elModalBackground = el('div', {class: 'panel-modal-background'});

  $(elModal).draggable({
    cancel: '.panel-modal-text,.panel-modal-title,.panel-modal-subtitle'
  });

  elTarget.innerHTML = '';
  elTarget.appendChild(elModal);
  if (opt.addBackground) {
    elTarget.appendChild(elModalBackground);
  }

  function close() {
    elModal.remove();
    if (opt.addBackground) {
      elModalBackground.remove();
    }
  }
}
