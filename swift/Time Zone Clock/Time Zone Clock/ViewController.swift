//
//  ViewController.swift
//  Time Zone Clock
//
//  Created by Tuomas Laakkonen on 24/07/2017.
//  Copyright © 2017 Laakkonen. All rights reserved.
//

import Cocoa

class ViewController: NSViewController {

    override func viewDidLoad() {
        super.viewDidLoad()

        // Do any additional setup after loading the view.
        localTime.dateValue = Date(timeIntervalSinceNow: 0);
        if offsetDirection.state == 1 {
            resultTime.dateValue = localTime.dateValue + offsetTime.dateValue.timeIntervalSince1970 + 3600;
        } else {
            resultTime.dateValue = localTime.dateValue - offsetTime.dateValue.timeIntervalSince1970 - 3600;
        }
    }

    override var representedObject: Any? {
        didSet {
        // Update the view, if already loaded.
        }
    }

    @IBAction func changeLocal(_ sender: NSDatePicker) {
        if offsetDirection.state == 1 {
            resultTime.dateValue = localTime.dateValue + offsetTime.dateValue.timeIntervalSince1970 + 3600;
        } else {
            resultTime.dateValue = localTime.dateValue - offsetTime.dateValue.timeIntervalSince1970 - 3600;
        }
    }
    
    @IBAction func changeOffset(_ sender: NSDatePicker) {
        if offsetDirection.state == 1 {
            resultTime.dateValue = localTime.dateValue + offsetTime.dateValue.timeIntervalSince1970 + 3600;
        } else {
            resultTime.dateValue = localTime.dateValue - offsetTime.dateValue.timeIntervalSince1970 - 3600;
        }
    }
    
    @IBAction func changeDirection(_ sender: NSButton) {
        if offsetDirection.state == 1 {
            resultTime.dateValue = localTime.dateValue + offsetTime.dateValue.timeIntervalSince1970 + 3600;
        } else {
            resultTime.dateValue = localTime.dateValue - offsetTime.dateValue.timeIntervalSince1970 - 3600;
        }
    }
    
    @IBOutlet weak var localTime: NSDatePicker!
    @IBOutlet weak var offsetTime: NSDatePicker!
    @IBOutlet weak var resultTime: NSDatePicker!
    @IBOutlet weak var offsetDirection: NSButton!
}

