use pixelengine as px;
use std::marker::PhantomData;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

pub struct Style {
    pub stroke: (f32, f32, f32, f32),
    pub foreground: (f32, f32, f32, f32),
    pub background: (f32, f32, f32, f32),
    pub text_size: f32,
    pub padding: f32
}

pub trait Widget<Message> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32);
    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32);
    fn click(&mut self, drag: bool, x: f32, y: f32) -> (Option<Message>, bool);
    fn apply_style(&mut self, style: &Style);
    fn send(&mut self, _event: Message) -> bool;
}

pub trait Update<Value> {
    fn update(&self, value: &mut Value);
    fn update_from(&mut self, value: &Value) -> bool;
}

impl<Message> Widget<Message> for Box<dyn Widget<Message>> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        self.deref().draw(window, x, y)
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        self.deref_mut().layout(window, max_width, max_height)
    }

    fn click(&mut self, drag: bool, x: f32, y: f32) -> (Option<Message>, bool) {
        self.deref_mut().click(drag, x, y)
    }

    fn apply_style(&mut self, style: &Style) {
        self.deref_mut().apply_style(style)
    }

    fn send(&mut self, event: Message) -> bool {
        self.deref_mut().send(event)
    }
}

pub trait WidgetExt: Sized {
    fn label<Message>(self, text: &str) -> Labelled<Text<Message>, Self, Message> {
        Labelled::new(Text::new(text), self)
    }

    fn style<Message>(mut self, style: &Style) -> Self where Self: Widget<Message> {
        self.apply_style(style);
        self
    }

    fn respond<Message, F: FnMut(&mut Self, Message) -> bool + 'static>(self, callback: F) -> Respond<Self, Message> {
        Respond { callback: Box::new(callback), widget: self }
    }

    fn on_click<Message, F: FnMut(&mut Self, bool, f32, f32) -> (Option<Message>, bool) + 'static>(self, callback: F) -> OnClick<Self, Message> {
        OnClick { callback: Box::new(callback), widget: self }
    }

    fn updates<Message, Value, F: Fn(&Self, &mut Value) + 'static, G: Fn(&mut Self, &Value) -> bool + 'static>(self, callback1: F, callback2: G) -> Updates<Self, Value, Message> {
        Updates { widget: self, callback1: Box::new(callback1), callback2: Box::new(callback2), phantom: PhantomData }
    }

    fn map<Before, After, A: FnMut(Before) -> Option<After> + 'static, B: FnMut(After) -> Option<Before> + 'static>(self, forward: A, backward: B) -> Map<Self, Before, After> {
        Map { forward: Box::new(forward), backward: Box::new(backward), inner: self }
    }

    fn mute<A, B>(self) -> Map<Self, B, A> {
        self.map(|_| None, |_| None)
    }

    fn boxed<Message>(self) -> Box<dyn Widget<Message>> where Self: Widget<Message> + 'static {
        Box::new(self)
    }

    fn pad<Message>(self) -> Padded<Self, Message> {
        Padded { inner: self, bounds: (0.0, 0.0), padding: 0.0, phantom: PhantomData }
    }
}

impl<T> WidgetExt for T {}

#[derive(Default)]
pub struct Text<Message> {
    pub text: String,
    pub fill: (f32, f32, f32, f32),
    pub size: f32,
    pub padding: f32,
    phantom: PhantomData<Message>
}

impl<Message> Text<Message> {
    pub fn new(text: &str) -> Text<Message> {
        Text { text: text.to_string(), phantom: PhantomData, fill: Default::default(), size: 0.0, padding: 0.0 }
    }
}

impl<Message> Widget<Message> for Text<Message> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        window.fill(self.fill.0, self.fill.1, self.fill.2, self.fill.3);
        window.text_size(self.size);
        window.text(self.text.as_ref(), x + self.padding, y + self.padding);
    }

    fn layout(&mut self, window: &px::Window, _max_width: f32, _max_height: f32) -> (f32, f32) {
        (window.text_width(self.text.as_ref(), Some(self.size)) + self.padding * 2.0, self.size + self.padding * 2.0)
    }

    fn click(&mut self, _drag: bool, _x: f32, _y: f32) -> (Option<Message>, bool) {
        (None, false)
    }

    fn apply_style(&mut self, style: &Style) {
        self.fill = style.stroke;
        self.size = style.text_size;
        self.padding = style.padding;
    }

    fn send(&mut self, _event: Message) -> bool {
        false
    }
}

pub struct Button<Inner, Message> {
    pub fill: Option<(f32, f32, f32, f32)>,
    pub stroke: Option<(f32, f32, f32, f32, f32)>,
    pub padding: f32,
    pub contents: Inner,
    bounds: (f32, f32),
    callback: Box<dyn FnMut() -> Option<Message>>
}

impl<Inner, Message> Button<Inner, Message> {
    pub fn new<F: FnMut() -> Option<Message> + 'static>(callback: F, contents: Inner) -> Button<Inner, Message> {
        Button {
            contents, callback: Box::new(callback), bounds: (0.0, 0.0),
            fill: Default::default(), stroke: Default::default(), padding: Default::default()
        }
    }
}

impl<Inner: Widget<Message>, Message> Widget<Message> for Button<Inner, Message> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        if let Some((r, g, b, a)) = self.fill {
            window.fill(r, g, b, a);
        } else {
            window.no_fill();
        }

        if let Some((r, g, b, a, w)) = self.stroke {
            window.stroke(r, g, b, a);
            window.stroke_weight(w);
        } else {
            window.no_stroke();
        }

        window.rect(x + self.padding, y + self.padding, self.bounds.0 - 2.0 * self.padding, self.bounds.1 - 2.0 * self.padding);

        self.contents.draw(window, x + self.padding, y + self.padding);
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        let (cw, ch) = self.contents.layout(window, max_width - 2.0 * self.padding, max_height - 2.0 * self.padding);
        self.bounds = (cw + 2.0 * self.padding, ch + 2.0 * self.padding);
        self.bounds
    }

    fn click(&mut self, drag: bool, x: f32, y: f32) -> (Option<Message>, bool) {
        if !drag && x >= self.padding && x <= self.bounds.0 - self.padding && y >= self.padding && y <= self.bounds.1 - self.padding {
            ((self.callback)(), false)
        } else {
            (None, false)
        }
    }

    fn apply_style(&mut self, style: &Style) {
        self.fill = Some(style.foreground);
        self.stroke = Some((style.stroke.0, style.stroke.1, style.stroke.2, style.stroke.3, 1.0));
        self.padding = style.padding;
        self.contents.apply_style(style);
    }

    fn send(&mut self, event: Message) -> bool {
        self.contents.send(event)
    }
}

pub struct Slider<Message> {
    pub foreground: (f32, f32, f32, f32),
    pub background: (f32, f32, f32, f32),
    pub stroke: (f32, f32, f32, f32),
    pub padding: f32,
    pub text_size: f32,
    pub min: f32,
    pub max: f32,
    pub value: f32,
    bounds: (f32, f32),
    callback: Box<dyn FnMut(f32) -> Option<Message>>
}

impl<Message> Slider<Message> {
    pub fn new<F: FnMut(f32) -> Option<Message> + 'static>(min: f32, max: f32, value: f32, callback: F) -> Slider<Message> {
        Slider {
            min, max, value, callback: Box::new(callback), bounds: (0.0, 0.0),
            foreground: Default::default(), background: Default::default(), stroke: Default::default(),
            padding: 0.0, text_size: 0.0
        }
    }
}

impl<Message> Widget<Message> for Slider<Message> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        window.stroke(self.stroke.0, self.stroke.1, self.stroke.2, self.stroke.3);
        window.fill(self.background.0, self.background.1, self.background.2, self.background.3);
        window.rect(x + self.padding, y + self.padding, self.bounds.0 - 2.0 * self.padding, self.bounds.1 - 2.0 * self.padding);
        window.fill(self.foreground.0, self.foreground.1, self.foreground.2, self.foreground.3);
        window.rect(x + self.padding, y + self.padding, (self.bounds.0 - 2.0 * self.padding) * (self.value - self.min)/ (self.max - self.min), self.bounds.1 - 2.0 * self.padding);
        window.text_size(self.text_size);
        window.fill(self.stroke.0, self.stroke.1, self.stroke.2, self.stroke.3);
        window.text(format!("{:3.2}", self.value).as_str(), x + 2.0 * self.padding, y + 2.0 * self.padding);
    }

    fn layout(&mut self, _window: &px::Window, max_width: f32, _max_height: f32) -> (f32, f32) {
        let height = self.text_size + self.padding * 4.0;
        self.bounds = (max_width, height);
        self.bounds
    }

    fn click(&mut self, _drag: bool, x: f32, _y: f32) -> (Option<Message>, bool) {
        if x > self.padding && x <= self.bounds.0 - self.padding {
            let value = ((x - self.padding)/ (self.bounds.0 - 2.0 * self.padding)) * (self.max - self.min) + self.min;
            self.value = value;
            ((self.callback)(value), false)
        } else {
            (None, false)
        }
    }

    fn apply_style(&mut self, style: &Style) {
        self.foreground = style.foreground;
        self.background = style.background;
        self.stroke = style.stroke;
        self.padding = style.padding;
        self.text_size = style.text_size;
    }

    fn send(&mut self, _event: Message) -> bool {
        false
    }
}

pub struct Stack<Message>(PhantomData<Message>);

impl<Message> Stack<Message> {
    pub fn new() -> Stack<Message> {
        Stack(PhantomData)
    }


    pub fn item<T>(self, next: T) -> StackItem<T, Message, Self>  {
        StackItem {
            fill: Default::default(), stroke: Default::default(),
            cbounds: (0.0, 0.0), bounds: (0.0, 0.0), phantom: PhantomData,
            contents: next, next: self
        }
    }
}

impl<Message> Widget<Message> for Stack<Message> {
    fn draw(&self, _window: &mut px::Window, _x: f32, _y: f32) {}

    fn layout(&mut self, _window: &px::Window, max_width: f32, _max_height: f32) -> (f32, f32) {
        (max_width, 0.0)
    }

    fn click(&mut self, _drag: bool, _x: f32, _y: f32) -> (Option<Message>, bool) {
        (None, false)
    }

    fn apply_style(&mut self, _style: &Style) {}

    fn send(&mut self, _event: Message) -> bool {
        false
    }
}

impl<Value, Message> Update<Value> for Stack<Message> {
    fn update(&self, _value: &mut Value) {}
    fn update_from(&mut self, _value: &Value) -> bool { false }
}

pub struct StackItem<Inner, Message, Next> {
    pub fill: Option<(f32, f32, f32, f32)>,
    pub stroke: Option<(f32, f32, f32, f32, f32)>,
    pub contents: Inner,
    pub next: Next,
    cbounds: (f32, f32),
    bounds: (f32, f32),
    phantom: PhantomData<Message>
}

impl<Inner, Message, Next> StackItem<Inner, Message, Next> {
    pub fn panel(self, text: &str, hidden: bool) -> Panel<Self, Message> {
        let closed = StackItem {
            fill: self.fill, stroke: self.stroke,
            cbounds: (0.0, 0.0), bounds: (0.0, 0.0), phantom: PhantomData,
            contents: Button::new(|| Some(None), Text::new("+")).label(text), next: Stack::new()
        };

        let open = StackItem {
            fill: self.fill, stroke: self.stroke,
            cbounds: (0.0, 0.0), bounds: (0.0, 0.0), phantom: PhantomData,
            contents: Button::new(|| Some(None), Text::new("-")).label(text), next: self.map(|x| Some(Some(x)), |x| x)
        };

        Panel {
            open, closed, hidden
        }
    }

    pub fn item<T>(self, next: T) -> StackItem<T, Message, Self>  {
        StackItem {
            fill: self.fill, stroke: self.stroke,
            cbounds: (0.0, 0.0), bounds: (0.0, 0.0), phantom: PhantomData,
            contents: next, next: self
        }
    }
}

impl<Value, Inner: Update<Value>, Message, Next: Update<Value>> Update<Value> for StackItem<Inner, Message, Next> {
    fn update(&self, value: &mut Value) {
        self.contents.update(value);
        self.next.update(value);
    }

    fn update_from(&mut self, value: &Value) -> bool {
        self.contents.update_from(value) | self.next.update_from(value)
    }
}

impl<Inner: Widget<Message>, Next: Widget<Message>, Message: Clone> Widget<Message> for StackItem<Inner, Message, Next> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        if let Some((r, g, b, a)) = self.fill {
            window.fill(r, g, b, a);
        } else {
            window.no_fill();
        }

        if let Some((r, g, b, a, w)) = self.stroke {
            window.stroke(r, g, b, a);
            window.stroke_weight(w);
        } else {
            window.no_stroke();
        }

        window.rect(x, y, self.bounds.0, self.bounds.1);

        self.contents.draw(window, x, y);
        self.next.draw(window, x, y + self.cbounds.1);
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        self.cbounds = self.contents.layout(window, max_width, max_height);
        let (_, nh) = self.next.layout(window, max_width, max_height - self.cbounds.1);
        self.bounds = (max_width, self.cbounds.1 + nh);
        self.bounds
    }

    fn click(&mut self, drag: bool, mx: f32, my: f32) -> (Option<Message>, bool) {
        if my < self.cbounds.1 && mx <= self.cbounds.0 {
            self.contents.click(drag, mx, my)
        } else {
            self.next.click(drag, mx, my - self.cbounds.1)
        }
    }

    fn apply_style(&mut self, style: &Style) {
        self.fill = Some(style.background);
        self.stroke = Some((style.stroke.0, style.stroke.1, style.stroke.2, style.stroke.3, 1.0));
        self.contents.apply_style(style);
        self.next.apply_style(style);
    }

    fn send(&mut self, event: Message) -> bool {
        self.contents.send(event.clone()) | self.next.send(event)
    }
}

pub struct Labelled<Label, Inner, Message> {
    pub label: Label,
    pub widget: Inner,
    phantom: PhantomData<Message>,
    height: f32,
    lheight: f32,
    wheight: f32,
    partition: f32,
    padding: f32
}

impl<Label, Inner, Message> Labelled<Label, Inner, Message> {
    pub fn new(label: Label, widget: Inner) -> Labelled<Label, Inner, Message> {
        Labelled {
            label, widget, height: 0.0, partition: 0.0, wheight: 0.0, padding: 0.0, phantom: PhantomData, lheight: 0.0
        }
    }
}

impl<Label: Widget<Message>, Inner: Widget<Message>, Message: Clone> Widget<Message> for Labelled<Label, Inner, Message> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        self.label.draw(window, x, y + 0.5 * (self.height - self.lheight));
        self.widget.draw(window, x + self.partition, y + 0.5 * (self.height - self.wheight));
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        let (lw, lh) = self.label.layout(window, max_width, max_height);
        self.lheight = lh;
        let (ww, wh) = self.widget.layout(window, max_width - lw - self.padding, max_height);
        self.wheight = wh;
        self.height = wh.max(lh);
        self.partition = lw + self.padding;
        ((ww + lw + self.padding), self.height)
    }

    fn click(&mut self, drag: bool, x: f32, y: f32) -> (Option<Message>, bool) {
        if x > self.partition {
            self.widget.click(drag, x - self.partition, y)
        } else {
            self.label.click(drag, x, y)
        }
    }

    fn apply_style(&mut self, style: &Style) {
        self.padding = style.padding;
        Widget::<Message>::apply_style(&mut self.label, style);
        self.widget.apply_style(style);
    }

    fn send(&mut self, event: Message) -> bool {
        self.label.send(event.clone()) | self.widget.send(event)
    }
}

pub struct Respond<Inner, Message> {
    callback: Box<dyn FnMut(&mut Inner, Message) -> bool>,
    widget: Inner
}

impl<Message, Inner: Widget<Message>> Widget<Message> for Respond<Inner, Message> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        self.widget.draw(window, x, y);
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        self.widget.layout(window, max_width, max_height)
    }

    fn click(&mut self, drag: bool, x: f32, y: f32) -> (Option<Message>, bool) {
        self.widget.click(drag, x, y)
    }

    fn apply_style(&mut self, style: &Style) {
        self.widget.apply_style(style);
    }

    fn send(&mut self, event: Message) -> bool {
        (self.callback)(&mut self.widget, event)
    }
}

pub struct Updates<Inner, Value, Message> {
    callback1: Box<dyn Fn(&Inner, &mut Value)>,
    callback2: Box<dyn Fn(&mut Inner, &Value) -> bool>,
    widget: Inner,
    phantom: PhantomData<Message>
}

impl<Message, Value, Inner: Widget<Message>> Widget<Message> for Updates<Inner, Value, Message> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        self.widget.draw(window, x, y);
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        self.widget.layout(window, max_width, max_height)
    }

    fn click(&mut self, drag: bool, x: f32, y: f32) -> (Option<Message>, bool) {
        self.widget.click(drag, x, y)
    }

    fn apply_style(&mut self, style: &Style) {
        self.widget.apply_style(style);
    }

    fn send(&mut self, event: Message) -> bool {
        self.widget.send(event)
    }
}

impl<Message, Value, Inner> Update<Value> for Updates<Inner, Value, Message> {
    fn update(&self, value: &mut Value) {
        (self.callback1)(&self.widget, value)
    }

    fn update_from(&mut self, value: &Value) -> bool {
        (self.callback2)(&mut self.widget, value)
    }
}

pub struct OnClick<Inner, Message> {
    callback: Box<dyn FnMut(&mut Inner, bool, f32, f32) -> (Option<Message>, bool)>,
    widget: Inner
}

impl<Message, Inner: Widget<Message>> Widget<Message> for OnClick<Inner, Message> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        self.widget.draw(window, x, y);
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        self.widget.layout(window, max_width, max_height)
    }

    fn click(&mut self, drag: bool, x: f32, y: f32) -> (Option<Message>, bool) {
        (self.callback)(&mut self.widget, drag, x, y)
    }

    fn apply_style(&mut self, style: &Style) {
        self.widget.apply_style(style);
    }

    fn send(&mut self, event: Message) -> bool {
        self.widget.send(event)
    }
}


pub struct Map<Inner, Before, After> {
    forward: Box<dyn FnMut(Before) -> Option<After>>,
    backward: Box<dyn FnMut(After) -> Option<Before>>,
    pub inner: Inner
}

impl<Inner: Widget<Before>, Before, After> Widget<After> for Map<Inner, Before, After> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        self.inner.draw(window, x, y);
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        self.inner.layout(window, max_width, max_height)
    }

    fn apply_style(&mut self, style: &Style) {
        self.inner.apply_style(style);
    }

    fn send(&mut self, event: After) -> bool {
        if let Some(event) = (self.backward)(event) {
            self.inner.send(event)
        } else {
            false
        }
    }

    fn click(&mut self, drag: bool, x: f32, y: f32) -> (Option<After>, bool) {
        let (event, reflow) = self.inner.click(drag, x, y);
        if let Some(event) = event {
            ((self.forward)(event), reflow)
        } else {
            (None, reflow)
        }
    }
}

pub struct Panel<Inner, Message> {
    open: StackItem<Labelled<Text<Option<Message>>, Button<Text<Option<Message>>, Option<Message>>, Option<Message>>, Option<Message>, Map<Inner, Message, Option<Message>>>,
    closed: StackItem<Labelled<Text<Option<Message>>, Button<Text<Option<Message>>, Option<Message>>, Option<Message>>, Option<Message>, Stack<Option<Message>>>,
    hidden: bool
}

impl<Inner: Widget<Message>, Message: Clone> Widget<Message> for Panel<Inner, Message> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        if self.hidden {
            self.closed.draw(window, x, y);
        } else {
            self.open.draw(window, x, y);
        }
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        if self.hidden {
            self.closed.layout(window, max_width, max_height)
        } else {
            self.open.layout(window, max_width, max_height)
        }
    }

    fn apply_style(&mut self, style: &Style) {
        self.closed.apply_style(style);
        self.open.apply_style(style);
    }

    fn send(&mut self, event: Message) -> bool {
        if self.hidden {
            self.closed.send(Some(event))
        } else {
            self.open.send(Some(event))
        }
    }

    fn click(&mut self, drag: bool, x: f32, y: f32) -> (Option<Message>, bool) {
        let (event, reflow) = if self.hidden {
            self.closed.click(drag, x, y)
        } else {
            self.open.click(drag, x, y)
        };

        if let Some(None) = event {
            self.hidden = !self.hidden;

            (None, true)
        } else if let Some(Some(event)) = event {
            (Some(event), reflow)
        } else {
            (None, reflow)
        }
    }
}

impl<Inner: Update<Value>, Value, Message> Update<Value> for Panel<Inner, Message> {
    fn update(&self, value: &mut Value) {
        self.open.next.inner.update(value)
    }

    fn update_from(&mut self, value: &Value) -> bool {
        self.open.next.inner.update_from(value)
    }
}

pub struct Selector {
    pub variants: Vec<(f32, f32, f32, &'static str, Button<Text<()>, ()>)>,
    bounds: (f32, f32),
    pub selected: &'static str,
    pub padding: f32,
    pub active: (f32, f32, f32, f32),
    pub inactive: (f32, f32, f32, f32)
}

impl Selector {
    pub fn new(variants: &[&'static str], selected: &'static str) -> Selector {
        Selector {
            variants: Iterator::map(variants.iter(), |name| (0.0, 0.0, 0.0, *name, Button::new(|| None, Text::new(name)))).collect(),
            bounds: (0.0, 0.0),
            selected, padding: 0.0, active: Default::default(), inactive: Default::default()
        }
    }
}

impl Widget<&'static str> for Selector {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        for (vx, _, _, _, variant) in &self.variants {
            variant.draw(window, x + *vx, y);
        }
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        let mut cx = 0.0;
        let mut mh = 0.0;
        for (x, w, h, _, item) in &mut self.variants {
            *x = cx;
            let (cw, ch) = item.layout(window, max_width - cx, max_height);
            if ch > mh {
                mh = ch;
            }
            *w = cw;
            *h = ch;
            cx += cw + self.padding;
        }
        self.bounds = (cx - self.padding, mh);
        self.bounds
    }

    fn click(&mut self, drag: bool, mx: f32, my: f32) -> (Option<&'static str>, bool) {
        let mut clicked = None;
        for (x, w, h, name, _) in &mut self.variants {
            if mx >= *x && mx < *x + *w && my <= *h {
                if !drag && *name != self.selected{
                    clicked = Some(*name);
                    break;
                }
            }
        }

        if let Some(clicked) = clicked {
            self.selected = clicked;
            for (_, _, _, name, item) in &mut self.variants {
                if *name == self.selected {
                    item.fill = Some(self.active);
                } else {
                    item.fill = Some(self.inactive);
                }
            }

            (Some(clicked), false)
        } else {
            (None, false)
        }
    }

    fn apply_style(&mut self, style: &Style) {
        self.padding = style.padding;
        self.active = style.foreground;
        self.inactive = style.background;
        
        for (_, _, _, name, item) in &mut self.variants {
            item.apply_style(style);
            if *name == self.selected {
                item.fill = Some(self.active);
            } else {
                item.fill = Some(self.inactive);
            }
        }
    }

    fn send(&mut self, _event: &'static str) -> bool {
        false
    }
}

pub struct Variants<Inner, Message> {
    pub active: StackItem<Map<Selector, &'static str, Result<Message, &'static str>>, Result<Message, &'static str>, StackItem<Map<Inner, Message, Result<Message, &'static str>>, Result<Message, &'static str>, Stack<Result<Message, &'static str>>>>,
    pub selected: &'static str,
    pub variants: HashMap<&'static str, StackItem<Map<Inner, Message, Result<Message, &'static str>>, Result<Message, &'static str>, Stack<Result<Message, &'static str>>>>
}

impl<Inner, Message> Variants<Inner, Message> {
    pub fn new(vars: Vec<(&'static str, Inner)>, selected: &'static str) -> Variants<Inner, Message> {
        let selector = Selector::new(&Iterator::map(vars.iter(), |(name, _)| *name).collect::<Vec<_>>(), selected).map(|x| Some(Err(x)), |_| None);
        let mut variants = HashMap::new();
        let mut active = None;
        for (name, variant) in vars {
            if name == selected {
                active = Some(Stack::new().item(variant.map(|x| Some(Ok(x)), |x| x.ok())));
            } else {
                variants.insert(name, Stack::new().item(variant.map(|x| Some(Ok(x)), |x| x.ok())));
            }
        }
        let active = active.unwrap().item(selector);
        Variants { active, variants, selected }
    }
}

impl<Inner: Widget<()>> Widget<()> for Variants<Inner, ()> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        self.active.draw(window, x, y);
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        self.active.layout(window, max_width, max_height)
    }

    fn apply_style(&mut self, style: &Style) {
        self.active.apply_style(style);
        for item in self.variants.values_mut() {
            item.apply_style(style);
        }
    }

    fn send(&mut self, event: ()) -> bool {
        let mut reflow = false;
        for item in self.variants.values_mut() {
            reflow |= item.send(Ok(event));
        }
        self.active.send(Ok(event)) | reflow
    }

    fn click(&mut self, drag: bool, x: f32, y: f32) -> (Option<()>, bool) {
        let (ev, reflow) = self.active.click(drag, x, y);

        if let Some(Err(selected)) = ev {
            if self.selected != selected {
                let new = self.variants.remove(selected).unwrap();
                let old = std::mem::replace(&mut self.active.next, new);
                self.variants.insert(self.selected, old);
                self.selected = selected;
                (Some(()), true)
            } else {
                (None, false)
            }
        } else if let Some(Ok(msg)) = ev {
            (Some(msg), reflow)
        } else {
            (None, false)
        }
    }
}

pub struct Padded<Inner, Message> {
    pub padding: f32,
    pub inner: Inner,
    bounds: (f32, f32),
    phantom: PhantomData<Message>
}

impl<Inner: Widget<Message>, Message> Widget<Message> for Padded<Inner, Message> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        self.inner.draw(window, x + self.padding, y + self.padding);
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        let (w, h) = self.inner.layout(window, max_width - 2.0 * self.padding, max_height - 2.0 * self.padding);
        self.bounds = (w + 2.0 * self.padding, h + 2.0 * self.padding);
        self.bounds
    }

    fn click(&mut self, drag: bool, x: f32, y: f32) -> (Option<Message>, bool) {
        if x >= self.padding && x <= self.bounds.0 - self.padding && y >= self.padding && y <= self.bounds.1 - self.padding {
            self.inner.click(drag, x - self.padding, y - self.padding)
        } else {
            (None, false)
        }
    }

    fn send(&mut self, event: Message) -> bool {
        self.inner.send(event)
    }

    fn apply_style(&mut self, style: &Style) {
        self.padding = style.padding;
        self.inner.apply_style(style);
    }
}

impl<Inner: Update<Value>, Value, Message> Update<Value> for Padded<Inner, Message> {
    fn update(&self, value: &mut Value) {
        self.inner.update(value);
    }

    fn update_from(&mut self, value: &Value) -> bool {
        self.inner.update_from(value)
    }
}

pub trait GUIBuilder {
    fn builder_internal(label: &'static str) -> Box<dyn UpdateWidget<(), Self>>;
    fn builder() -> Box<dyn UpdateWidget<(), Self>>;
}

pub trait UpdateWidget<Message, Value>: Widget<Message> + Update<Value> {}

impl<Message, Value, T> UpdateWidget<Message, Value> for T where T: Widget<Message> + Update<Value> {}

impl<Message, Value> Widget<Message> for Box<dyn UpdateWidget<Message, Value>> {
    fn draw(&self, window: &mut px::Window, x: f32, y: f32) {
        self.deref().draw(window, x, y)
    }

    fn layout(&mut self, window: &px::Window, max_width: f32, max_height: f32) -> (f32, f32) {
        self.deref_mut().layout(window, max_width, max_height)
    }

    fn click(&mut self, drag: bool, x: f32, y: f32) -> (Option<Message>, bool) {
        self.deref_mut().click(drag, x, y)
    }

    fn apply_style(&mut self, style: &Style) {
        self.deref_mut().apply_style(style)
    }

    fn send(&mut self, event: Message) -> bool {
        self.deref_mut().send(event)
    }
}

impl<Message, Value> Update<Value> for Box<dyn UpdateWidget<Message, Value>> {
    fn update(&self, value: &mut Value) {
        self.deref().update(value)
    }

    fn update_from(&mut self, value: &Value) -> bool {
        self.deref_mut().update_from(value)
    }
}

#[macro_export]
macro_rules! impl_builder_pat {
    (#[slider($(min:)? $min:expr, $(max:)? $max:expr, $(default:)? $default:expr)], $ename:ident, $vname:ident, $fname:ident: $fty:ty) => {
        gui::Slider::new($min, $max, $default, |_| Some(())).label(stringify!($fname)).updates(|slider, value| match value {
            $ename::$vname { ref mut $fname, .. } => *$fname = slider.widget.value,
            _ => ()
        }, |slider, value| match value {
            $ename::$vname { $fname, .. } => {
                slider.widget.value = *$fname;
                false
            },
            _ => false
        })
    };
    (#[slider($(min:)? $min:expr, $(max:)? $max:expr, $(default:)? $default:expr)], $tname:ident, $fname:ident: $fty:ty) => {
        gui::Slider::new($min, $max, $default, |_| Some(())).label(stringify!($fname)).updates(|slider, value: &mut $tname| value.$fname = slider.widget.value, |slider, value: &$tname| {
            slider.widget.value = value.$fname;
            false
        })
    };
    (, $tname:ident, $fname:ident: $fty:ty) => {
        <$fty>::builder_internal(stringify!($fname)).updates(|inner, value: &mut $tname| {
            inner.update(&mut value.$fname)
        }, |inner, value: &$tname| {
            inner.update_from(&value.$fname)
        })
    }
}

#[macro_export]
macro_rules! GUIBuilder {
    {
        $(#[$meta:meta])*
        $pub:vis struct $name:ident {
            $(
                $(#[$($t:tt)+])?
                $fname:ident: $fty:ty
            ),*
        }
    } => {
        $(#[$meta])*
        $pub struct $name {
            $(
                $fname: $fty
            ),*
        }

        impl crate::gui::GUIBuilder for $name {
            fn builder() -> Box<dyn crate::gui::UpdateWidget<(), $name>> {
                Self::builder_internal(stringify!($name))
            }

            fn builder_internal(label: &'static str) -> Box<dyn crate::gui::UpdateWidget<(), $name>> {
                use crate::gui;
                Box::new(gui::Stack::new()
                    $(.item(impl_builder_pat!($(#[$($t)+])?, $name, $fname: $fty)))*
                    .panel(label, true)
                    .pad())
            }
        }
    };
    {
        $(#[$meta:meta])*
        $pub:vis enum $name:ident {
            $($vname:ident $({
                $(
                    $(#[$($t:tt)+])?
                    $fname:ident: $fty:ty
                ),*
            })?),*
        }
    } => {
        $(#[$meta])*
        $pub enum $name {
            $($vname $({
                $(
                    $fname: $fty
                ),*
            })?),+
        }

        impl crate::gui::GUIBuilder for $name {
            fn builder() -> Box<dyn crate::gui::UpdateWidget<(), $name>> {
                Self::builder_internal(stringify!($name))
            }

            #[allow(unused_variables)]
            fn builder_internal(label: &'static str) -> Box<dyn crate::gui::UpdateWidget<(), $name>> {
                use crate::gui;
                let variants = vec![
                    $(
                        (stringify!($vname),
                            Box::new(gui::Stack::new()
                                $($(.item(impl_builder_pat!($(#[$($t)+])?, $name, $vname, $fname: $fty)))*)?) as Box<dyn crate::gui::UpdateWidget<(), $name>>
                        )
                    ),+
                ];
                let selected = variants[0].0;
                Box::new(gui::Stack::new()
                    .item(gui::Variants::new(variants, selected).updates(|variants, val| {
                        match variants.selected {
                            $(
                                stringify!($vname) => *val = $name::$vname $({ $($fname: Default::default()),* })?,
                            )+
                            _ => ()
                        }
                        variants.active.next.contents.inner.update(val);
                    }, |variants, val| {
                        match val {
                            $(
                                $name::$vname $({ $($fname),* })? => {
                                    let selected = stringify!($vname);
                                    let reflow = if variants.selected != selected {
                                        let new = variants.variants.remove(selected).unwrap();
                                        let old = std::mem::replace(&mut variants.active.next, new);
                                        variants.variants.insert(variants.selected, old);
                                        variants.selected = selected;

                                        for (_, _, _, name, item) in &mut variants.active.contents.inner.variants {
                                            if *name == selected {
                                                item.fill = Some(variants.active.contents.inner.active);
                                            } else {
                                                item.fill = Some(variants.active.contents.inner.inactive);
                                            }
                                        }
                                        variants.active.contents.inner.selected = selected;

                                        true
                                    } else {
                                        false
                                    };

                                    variants.active.next.contents.inner.update_from(val) | reflow
                                }
                            )+
                        }
                    }))
                    .panel(label, true)
                    .pad())
            }
        }
    }
}
